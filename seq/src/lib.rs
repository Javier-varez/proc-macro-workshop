use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, Token};

struct SeqItem {
    _ident: syn::Ident,
    _start: syn::LitInt,
    _end: syn::LitInt,
}

impl Parse for SeqItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<SeqItem> {
        let ident: syn::Ident = input.parse()?;
        let _: Token![in] = input.parse()?;
        let start: syn::LitInt = input.parse()?;
        let _: Token![..] = input.parse()?;
        let end: syn::LitInt = input.parse()?;
        let _: syn::ExprBlock = input.parse()?;

        Ok(SeqItem {
            _ident: ident,
            _start: start,
            _end: end,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(input as SeqItem);

    let tokens = quote! {
        pub fn test() {}
    };

    TokenStream::from(tokens)
}
