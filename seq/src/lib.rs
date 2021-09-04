use proc_macro::TokenStream;
use syn::{braced, parse::Parse, parse_macro_input, Token};

struct SeqItem {
    _ident: syn::Ident,
    _start: syn::LitInt,
    _end: syn::LitInt,
    _body: proc_macro2::TokenStream,
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
            _ident: ident,
            _start: start,
            _end: end,
            _body: body,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let _seq_item = parse_macro_input!(input as SeqItem);

    TokenStream::new()
}
