use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parser, parse_macro_input};

#[proc_macro_attribute]
pub fn attrs(args: TokenStream, input: TokenStream) -> TokenStream {
    if !args.is_empty() {
        return syn::Error::new_spanned(
            proc_macro2::token_stream::TokenStream::from(args),
            "unexpected argument",
        )
        .to_compile_error()
        .into();
    }

    let item_struct = parse_macro_input!(input as syn::ItemStruct);
    if let Err(e) = check_struct(&item_struct) {
        let mut out = item_struct.to_token_stream();
        out.extend(e.to_compile_error());
        out.into()
    } else {
        add_attrs(item_struct)
    }
}

fn check_struct(item_struct: &syn::ItemStruct) -> Result<(), syn::Error> {
    // Check that there is no attrs field
    for field in &item_struct.fields {
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
}

fn add_attrs(mut item_struct: syn::ItemStruct) -> TokenStream {
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
