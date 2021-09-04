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
    fn expand(&self) -> proc_macro2::TokenStream {
        let start: u32 = self.start.base10_parse().unwrap();
        let end: u32 = self.end.base10_parse().unwrap();
        let body = &self.body;

        let mut stream = TokenStream::new();
        for i in start..end {
            let replacement_identifier = TokenTree::Literal(Literal::u32_unsuffixed(i));
            stream.extend(
                body.clone()
                    .into_iter()
                    .map(|token| self.map_identifier_recursive(&replacement_identifier, token)),
            );
        }
        stream
    }

    fn map_identifier_recursive(&self, replacement: &TokenTree, tree: TokenTree) -> TokenTree {
        match tree {
            TokenTree::Ident(ident) if ident == self.ident => replacement.clone(),
            TokenTree::Group(group) => {
                let mut stream = TokenStream::new();
                stream.extend(
                    group
                        .stream()
                        .clone()
                        .into_iter()
                        .map(|token| self.map_identifier_recursive(replacement, token)),
                );

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
