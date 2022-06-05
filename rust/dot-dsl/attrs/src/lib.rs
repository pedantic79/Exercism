use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parser, parse_macro_input, spanned::Spanned};

#[proc_macro_attribute]
pub fn attrs(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut out = input.clone();

    let item = parse_macro_input!(input as syn::Item);
    assert!(args.is_empty());

    if let Err(e) = check_struct(item) {
        out.extend(TokenStream::from(e.to_compile_error()));
        out
    } else {
        add_attrs(out)
    }
}

fn add_attrs(input: TokenStream) -> TokenStream {
    let mut item_struct = parse_macro_input!(input as syn::ItemStruct);

    // Add attrs attribute to struct
    if let syn::Fields::Named(fields) = &mut item_struct.fields {
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! { pub attrs: ::std::collections::HashMap<String, String> })
                .unwrap(),
        );
    }

    // Add impl #name { fn with_attrs }
    let name = &item_struct.ident;
    let mut out = item_struct.to_token_stream();
    out.extend(quote! {
        impl #name {
            pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
                attrs.iter().fold(self, |mut s, (k, v)| {
                    s.attrs.insert(k.to_string(), v.to_string());
                    s
                })
            }
        }
    });

    out.into()
}

fn check_struct(input: syn::Item) -> Result<(), syn::Error> {
    if let syn::Item::Struct(s) = &input {
        // Check that there is no attrs field
        for field in &s.fields {
            if let Some(ident) = &field.ident {
                if ident.to_string().as_str() == "attrs" {
                    return Err(syn::Error::new(
                        ident.span(),
                        "struct must not have `attrs` field",
                    ));
                }
            }
        }

        Ok(())
    } else {
        Err(syn::Error::new(input.span(), "expected struct"))
    }
}
