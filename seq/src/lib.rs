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

fn map_identifier_recursive(
    expected_ident: &syn::Ident,
    replacement: &TokenTree,
    tree: TokenTree,
) -> TokenTree {
    match tree {
        TokenTree::Ident(ident) if ident == *expected_ident => replacement.clone(),
        TokenTree::Group(group) => {
            let mut stream = TokenStream::new();
            stream.extend(
                group
                    .stream()
                    .clone()
                    .into_iter()
                    .map(|token| map_identifier_recursive(expected_ident, replacement, token)),
            );

            let new_group = Group::new(group.delimiter(), stream);
            TokenTree::Group(new_group)
        }
        tt => tt,
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq_item = parse_macro_input!(input as SeqItem);

    let start: u32 = seq_item.start.base10_parse().unwrap();
    let end: u32 = seq_item.end.base10_parse().unwrap();
    let body = seq_item.body;
    let loop_ident = seq_item.ident;

    let mut stream = TokenStream::new();
    for i in start..end {
        let replacement_identifier = TokenTree::Literal(Literal::u32_unsuffixed(i));
        stream.extend(
            body.clone()
                .into_iter()
                .map(|token| map_identifier_recursive(&loop_ident, &replacement_identifier, token)),
        );
    }

    stream.into()
}
