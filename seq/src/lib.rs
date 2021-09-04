use proc_macro2::{Group, Literal, TokenStream, TokenTree};
use syn::{braced, parse::Parse, parse_macro_input, Token};

struct SeqItem {
    ident: syn::Ident,
    start: syn::LitInt,
    end: syn::LitInt,
    body: TokenStream,
}

impl Parse for SeqItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<SeqItem> {
        let content;
        let ident: syn::Ident = input.parse()?;
        let _: Token![in] = input.parse()?;
        let start: syn::LitInt = input.parse()?;
        let _: Token![..] = input.parse()?;
        let end: syn::LitInt = input.parse()?;
        let _ = braced!(content in input);
        let body = content.parse()?;

        Ok(SeqItem {
            ident,
            start,
            end,
            body,
        })
    }
}

impl SeqItem {
    fn has_repetition_group(&self, stream: &TokenStream) -> bool {
        let mut iter = stream.clone().into_iter();

        while let Some(token) = iter.next() {
            match token {
                TokenTree::Punct(punct) if punct.as_char() == '#' => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
                            if let Some(TokenTree::Punct(punct)) = iter.next() {
                                if punct.as_char() == '*' {
                                    // Found repetition group
                                    return true;
                                }
                            }
                        }
                    }
                }
                TokenTree::Group(group) => {
                    if self.has_repetition_group(&group.stream()) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        return false;
    }

    fn map_repetition_group(
        &self,
        token: TokenTree,
        remaining: &mut proc_macro2::token_stream::IntoIter,
    ) -> TokenStream {
        match token {
            TokenTree::Punct(punct) if punct.as_char() == '#' => {
                let mut lookup = remaining.clone();
                if let Some(TokenTree::Group(group)) = lookup.next() {
                    if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
                        if let Some(TokenTree::Punct(punct)) = lookup.next() {
                            remaining.next().unwrap();
                            remaining.next().unwrap();
                            if punct.as_char() == '*' {
                                let mut stream = TokenStream::new();
                                // Repeat this N times
                                let start: u32 = self.start.base10_parse().unwrap();
                                let end: u32 = self.end.base10_parse().unwrap();
                                for i in start..end {
                                    let replacement_identifier =
                                        TokenTree::Literal(Literal::u32_unsuffixed(i));
                                    let mut inner_iter = group.stream().into_iter();
                                    while let Some(token) = inner_iter.next() {
                                        stream.extend(std::iter::once(
                                            self.map_identifier_recursive(
                                                &replacement_identifier,
                                                token,
                                                &mut inner_iter,
                                            ),
                                        ));
                                    }
                                }

                                stream
                            } else {
                                panic!("Invalid group found");
                            }
                        } else {
                            panic!("Invalid group #() found");
                        }
                    } else {
                        TokenStream::from(TokenTree::Punct(punct))
                    }
                } else {
                    TokenStream::from(TokenTree::Punct(punct))
                }
            }
            TokenTree::Group(group) => {
                let mut stream = TokenStream::new();
                let mut inner_iter = group.stream().into_iter();
                while let Some(token) = inner_iter.next() {
                    stream.extend(self.map_repetition_group(token, &mut inner_iter));
                }
                let new_group =
                    TokenTree::Group(proc_macro2::Group::new(group.delimiter(), stream));
                TokenStream::from(new_group)
            }
            tt => TokenStream::from(tt),
        }
    }

    fn expand_with_repetition_group(&self) -> TokenStream {
        let mut stream = TokenStream::new();
        let mut iter = self.body.clone().into_iter();
        while let Some(token) = iter.next() {
            stream.extend(self.map_repetition_group(token, &mut iter));
        }
        stream
    }

    fn expand(&self) -> TokenStream {
        let start: u32 = self.start.base10_parse().unwrap();
        let end: u32 = self.end.base10_parse().unwrap();

        // eprintln!("{:#?}", self.body);

        if !self.has_repetition_group(&self.body) {
            let mut stream = TokenStream::new();
            for i in start..end {
                let replacement_identifier = TokenTree::Literal(Literal::u32_unsuffixed(i));
                let mut iter = self.body.clone().into_iter();
                while let Some(token) = iter.next() {
                    stream.extend(std::iter::once(self.map_identifier_recursive(
                        &replacement_identifier,
                        token,
                        &mut iter,
                    )));
                }
            }
            return stream;
        } else {
            return self.expand_with_repetition_group();
        }
    }

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

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq_item = parse_macro_input!(input as SeqItem);
    seq_item.expand().into()
}
