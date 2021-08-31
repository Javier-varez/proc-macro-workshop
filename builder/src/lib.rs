use proc_macro::TokenStream;

use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn get_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        if let Some(syn::PathSegment {
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
            ..
        }) = path.segments.first()
        {
            let arg = args.first().unwrap();
            if let syn::GenericArgument::Type(ty) = arg {
                Some(ty)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", ast);

    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let field_is_option = |f: &syn::Field| {
        if let syn::Type::Path(syn::TypePath { ref path, .. }) = f.ty {
            return path.segments.len() == 1 && path.segments[0].ident == "Option";
        }
        false
    };

    let optionized = fields.iter().map(|f| {
        let orig_field = f.clone();
        let name = &orig_field.ident;
        let ty = &orig_field.ty;
        if field_is_option(&orig_field) {
            quote! {
                #orig_field
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>
            }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if field_is_option(&f) {
            if let Some(inner) = get_inner_type(ty) {
                quote! {
                    pub fn #name(&mut self, #name: #inner) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            } else {
                panic!("Could not find inner type for the Option");
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: None
        }
    });

    let build_tokens = fields.iter().map(|f| {
        let name = &f.ident;
        if field_is_option(&f) {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let expanded = quote! {
        impl #name {
            pub fn builder() -> #bident {
                #bident {
                    #(#builder_fields,)*
                }
            }
        }

        pub struct #bident {
            #(#optionized,)*
        }

        impl #bident {
            #(#methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_tokens,)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
