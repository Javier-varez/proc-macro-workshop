use proc_macro2::{Group, Literal, TokenStream, TokenTree};
use syn::{braced, parse::Parse, parse_macro_input, Token};

/// Represents all of the operating modes of the seq! macro.
enum Mode {
    /// RepeatBody will repeat the contens of the body as many times as specified in the range.
    /// All matching identifiers inside of the body will be replaced by the number of the iteration.
    RepeatBody,
    /// RepeatGroup will repeat only the contents of the given group enclosed by `#()*`.
    /// All matching identifiers inside of the group will be replaced by the number of the iteration.
    RepeatGroup,
}

/// Abstraction over a range like 1..10 or an inclusive range like 1..=10
enum Range {
    Inclusive(u64, u64),
    Exclusive(u64, u64),
}

impl Range {
    /// Returns a standard range, adapting an inclusive range into an exclusive range
    fn iter(&self) -> std::ops::Range<u64> {
        match self {
            Range::Inclusive(start, end) => *start..*end + 1,
            Range::Exclusive(start, end) => *start..*end,
        }
    }
}

impl Parse for Range {
    /// Parses a Range from a ParseStream
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Range> {
        let start: syn::LitInt = input.parse()?;
        let _: Token![..] = input.parse()?;
        let mut inclusive = false;
        if let Some(_) = input.parse::<Token![=]>().ok() {
            inclusive = true;
        }
        let end: syn::LitInt = input.parse()?;

        let start: u64 = start.base10_parse().unwrap();
        let end: u64 = end.base10_parse().unwrap();

        if inclusive {
            Ok(Range::Inclusive(start, end))
        } else {
            Ok(Range::Exclusive(start, end))
        }
    }
}

/// Abstraction over the syntax of the seq! macro.
struct SeqItem {
    ident: syn::Ident,
    range: Range,
    body: TokenStream,
    mode: Mode,
}

impl Parse for SeqItem {
    /// Parses a SeqItem from a ParseStream
    fn parse(input: syn::parse::ParseStream) -> syn::Result<SeqItem> {
        let content;

        let ident: syn::Ident = input.parse()?;
        let _: Token![in] = input.parse()?;

        let range: Range = input.parse()?;

        let _ = braced!(content in input);
        let body = content.parse()?;

        let mode = Self::parse_mode(&body);

        Ok(SeqItem {
            ident,
            range,
            body,
            mode,
        })
    }
}

impl SeqItem {
    /// Traverses the token stream recursively looking for repetition groups. If any repetition
    /// groups is found it returns Mode::RepeatGroup. Otherwise, it returns Mode::RepeatBody
    fn parse_mode(stream: &TokenStream) -> Mode {
        let mut iter = stream.clone().into_iter();

        while let Some(token) = iter.next() {
            match token {
                TokenTree::Punct(punct) if punct.as_char() == '#' => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
                            if let Some(TokenTree::Punct(punct)) = iter.next() {
                                if punct.as_char() == '*' {
                                    return Mode::RepeatGroup;
                                }
                            }
                        }
                    }
                }
                TokenTree::Group(group) => match Self::parse_mode(&group.stream()) {
                    Mode::RepeatGroup => return Mode::RepeatGroup,
                    _ => {}
                },
                _ => {}
            }
        }
        return Mode::RepeatBody;
    }

    /// Looks for repetition groups `#()*` and replaces them by the specified sequence.
    ///
    /// This function takes a single token tree with an iterator of the remaining token stream.
    /// Upon finding a token group it will replace it by a token stream containing the stream of
    /// tokens specified inside the group.
    ///
    /// All other token trees not belonging to a repetition group will be returned as-is wrapped in
    /// a token stream.
    ///
    /// This function recurses into groups to make sure that all repetition groups in the macro are
    /// replaced, not just the top-level ones
    fn map_repetition_groups(
        &self,
        token: TokenTree,
        remaining: &mut proc_macro2::token_stream::IntoIter,
    ) -> syn::Result<TokenStream> {
        match token {
            TokenTree::Punct(punct) if punct.as_char() == '#' => {
                let mut lookup = remaining.clone();
                if let Some(TokenTree::Group(group)) = lookup.next() {
                    if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
                        if let Some(TokenTree::Punct(punct)) = lookup.next() {
                            if punct.as_char() == '*' {
                                // At this point it is clear that we have detected a repetition group.
                                // We consume the tokens that belong to it and then parse the body of
                                // the group down to the expand function to repeat it as many times as
                                // requested
                                remaining.next().unwrap();
                                remaining.next().unwrap();
                                return Ok(self.expand_stream(&group.stream()));
                            } else {
                                panic!("Invalid group found. A repetition group should have the form `#()*`");
                            }
                        } else {
                            panic!("Invalid group found. A repetition group should have the form `#()*`");
                        }
                    }
                }
                return Ok(TokenStream::from(TokenTree::Punct(punct)));
            }
            TokenTree::Group(group) => {
                // Recurse into each of the groups found so that we make sure all repetition groups
                // get replaced
                let mut stream = TokenStream::new();
                let mut inner_iter = group.stream().into_iter();
                while let Some(token) = inner_iter.next() {
                    stream.extend(self.map_repetition_groups(token, &mut inner_iter)?);
                }
                let new_group =
                    TokenTree::Group(proc_macro2::Group::new(group.delimiter(), stream));
                Ok(TokenStream::from(new_group))
            }
            tt => Ok(TokenStream::from(tt)),
        }
    }

    /// Expands the given stream, looking for identifiers to replace.
    fn expand_stream(&self, template_stream: &TokenStream) -> TokenStream {
        let mut stream = TokenStream::new();
        for i in self.range.iter() {
            let replacement_identifier = TokenTree::Literal(Literal::u64_unsuffixed(i));
            let mut iter = template_stream.clone().into_iter();
            while let Some(token) = iter.next() {
                stream.extend(std::iter::once(self.map_identifier_recursive(
                    &replacement_identifier,
                    token,
                    &mut iter,
                )));
            }
        }
        stream
    }

    /// Expands the complete SeqItem into a single TokenStream
    fn expand(&self) -> TokenStream {
        match self.mode {
            Mode::RepeatBody => self.expand_stream(&self.body),
            Mode::RepeatGroup => {
                let mut stream = TokenStream::new();
                let mut iter = self.body.clone().into_iter();
                while let Some(token) = iter.next() {
                    stream.extend(self.map_repetition_groups(token, &mut iter));
                }
                stream
            }
        }
    }

    /// Maps a TokenTree into its replacement or itself if it is not supposed to be replaced.
    /// Sometimes more than one TokenTree is replaced, which is why we also pass the iterator with
    /// the remaining Tokens.
    ///
    ///   - When finding a `N` token it will replace it by the passed replacement token tree.
    ///   - When finding a `Irq#N` it will replace `N` it by the passed replacement token tree and join
    /// it with the previous identifier into a single identifier. This allows the user to
    /// concatenate into an identifier.
    ///
    fn map_identifier_recursive(
        &self,
        replacement: &TokenTree,
        tree: TokenTree,
        remaining: &mut proc_macro2::token_stream::IntoIter,
    ) -> TokenTree {
        match tree {
            TokenTree::Ident(ident) if ident == self.ident => replacement.clone(),
            TokenTree::Ident(first_ident) => {
                let mut lookup_iter = remaining.clone();
                match lookup_iter.next() {
                    Some(TokenTree::Punct(punct)) if punct.as_char() == '#' => {
                        match lookup_iter.next() {
                            Some(TokenTree::Ident(ident)) if ident == self.ident => {
                                // Matched arguments, let's consume them from the original iterator
                                remaining.next().unwrap();
                                remaining.next().unwrap();

                                let actual_str = format!("{}{}", first_ident, replacement);
                                let actual_ident =
                                    proc_macro2::Ident::new(&actual_str, first_ident.span());

                                TokenTree::Ident(actual_ident)
                            }
                            _ => TokenTree::Ident(first_ident),
                        }
                    }
                    _ => TokenTree::Ident(first_ident),
                }
            }
            TokenTree::Group(group) => {
                let mut stream = TokenStream::new();

                let mut iter = group.stream().into_iter();
                while let Some(token) = iter.next() {
                    stream.extend(std::iter::once(self.map_identifier_recursive(
                        replacement,
                        token,
                        &mut iter,
                    )));
                }

                let new_group = Group::new(group.delimiter(), stream);
                TokenTree::Group(new_group)
            }
            tt => tt,
        }
    }
}

/// The seq! proc macro. Expands the given body a number of times replacing a given identifier.
///
/// ```rust
///     use seq::seq;
///     seq!(N in 0..16 {
///         #[derive(Copy, Clone, Debug, PartialEq)]
///         enum Interrupt {
///             #(
///                 Irq#N,
///             )*
///         }
///     });
/// ```
#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq_item = parse_macro_input!(input as SeqItem);
    seq_item.expand().into()
}
