use proc_macro::TokenStream;

use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

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

    let field_is_vec = |f: &syn::Field| {
        if let syn::Type::Path(syn::TypePath { ref path, .. }) = f.ty {
            return path.segments.len() == 1 && path.segments[0].ident == "Vec";
        }
        false
    };

    let optionized = fields.iter().map(|f| {
        let mut orig_field = f.clone();
        orig_field.attrs = vec![];
        let name = &orig_field.ident;
        let ty = &orig_field.ty;
        if field_is_option(&orig_field) || field_is_vec(&orig_field) {
            quote! {
                #orig_field
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>
            }
        }
    });

    let methods = fields.iter().filter_map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        // Let's get the supported attributes for the current field
        let attributes = match parse_attributes(&f.attrs) {
            Ok(attrs) => attrs,
            Err(token_stream) => {
                return Some(token_stream.to_compile_error());
            }
        };

        let each_attr = attributes.iter().find_map(|e| match e {
            BuilderAttribute::Each(name) => Some(name),
        });

        if field_is_option(&f) {
            if let Some(inner) = get_inner_type(ty) {
                Some(quote! {
                    pub fn #name(&mut self, #name: #inner) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                })
            } else {
                panic!("Could not find inner type for the Option");
            }
        } else if field_is_vec(&f) {
            if let Some(each_name) = each_attr {
                let name = name.as_ref().unwrap().to_string();
                if each_name.eq(&name) {
                    // We don't generate the method since it would conflict with the extension
                    return None;
                }
            }

            Some(quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            })
        } else {
            Some(quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            })
        }
    });

    let method_ext = fields.iter().filter_map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        // // Let's get the supported attributes for the current field

        if field_is_vec(&f) {
            let attributes = match parse_attributes(&f.attrs) {
                Ok(attrs) => attrs,
                Err(token_stream) => {
                    return Some(token_stream.to_compile_error());
                }
            };

            let each_attr = attributes.iter().find_map(|e| match e {
                BuilderAttribute::Each(name) => Some(name),
            });

            let inner_type = get_inner_type(ty).expect("Unable to find inner type");

            if let Some(each_name) = each_attr {
                let each_name = syn::Ident::new(each_name, name.as_ref().unwrap().span());
                Some(quote! {
                    pub fn #each_name(&mut self, #each_name: #inner_type) -> &mut Self {
                        self.#name.push(#each_name);
                        self
                    }
                })
            } else {
                // The vec has no requested extension method,
                // so there's no need to generate anything
                None
            }
        } else {
            // If it is not a vector, there are no extension methods
            None
        }
    });

    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if field_is_vec(&f) {
            quote! {
                #name: vec![]
            }
        } else {
            quote! {
                #name: None
            }
        }
    });

    let build_tokens = fields.iter().map(|f| {
        let name = &f.ident;
        if field_is_option(&f) || field_is_vec(&f) {
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
            #(#method_ext)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_tokens,)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}

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

enum BuilderAttribute {
    Each(String),
}

fn parse_attributes(attrs: &Vec<syn::Attribute>) -> syn::Result<Vec<BuilderAttribute>> {
    let mut result: Vec<BuilderAttribute> = vec![];
    for attr in attrs {
        let meta = attr
            .parse_meta()
            .expect("Error parsing metadata of the 'builder' attribute");
        match meta {
            syn::Meta::List(syn::MetaList {
                ref path,
                ref nested,
                ..
            }) => {
                if path.segments.len() != 1 || path.segments[0].ident != "builder" {
                    return Err(syn::Error::new(
                        meta.span(),
                        "expected `builder(each = \"...\")`",
                    ));
                }

                if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                    path: path2,
                    lit,
                    ..
                }))) = nested.first()
                {
                    if path2.segments.len() != 1 || path2.segments[0].ident != "each" {
                        return Err(syn::Error::new_spanned(
                            meta,
                            "expected `builder(each = \"...\")`",
                        ));
                    }

                    if let syn::Lit::Str(lit) = lit {
                        result.push(BuilderAttribute::Each(lit.value()));
                    } else {
                        panic!("Invalid literal type found in attribute");
                    }
                }
            }
            x => {
                panic!(
                    "Invalid type of inert attribute for builder was found: {:?}",
                    x
                );
            }
        }
    }

    Ok(result)
}
