use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, Lifetime};

#[proc_macro_derive(Location)]
pub fn location_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    impl_location_macro(&ast)
}

#[proc_macro_derive(CloneIn)]
pub fn clone_in_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    impl_clone_in_macro(&ast)
}

fn impl_location_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    let Data::Enum(data) = &ast.data else {
        panic!("Location can only be derived for enums");
    };

    let match_arms = data.variants.iter().map(|variant| {
        let variant_name = &variant.ident;

        if variant.fields.len() != 1 {
            panic!("Location can only be derived for enums with a single field");
        }

        quote! {
            Self::#variant_name(inner) => inner.start
        }
    });

    let match_arms_clone = match_arms.clone();

    let gen = quote! {
        impl<'a> Location for #name<'a> {
            fn start(&self) -> u32 {
                match self {
                    #(#match_arms),*
                }
            }

            fn end(&self) -> u32 {
                match self {
                     #(#match_arms_clone),*
                }
            }
        }
    };

    gen.into()
}

fn impl_clone_in_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    if let Data::Enum(data) = &ast.data {
        let match_arms = data.variants.iter().map(|variant| {
            let variant_name = &variant.ident;

            if variant.fields.len() == 0 {
                quote! {
                    Self::#variant_name => Self::#variant_name
                }
            } else if variant.fields.len() == 1 {
                quote! {
                    Self::#variant_name(inner) => {
                        Self::#variant_name(inner.clone_in(arena))
                    }
                }
            } else {
                panic!("CloneIn can only be derived for enums with no or single field(s)");
            }
        });

        let lifetimes: Vec<&Lifetime> = ast
            .generics
            .params
            .iter()
            .filter_map(|param| {
                if let syn::GenericParam::Lifetime(lifetime) = param {
                    Some(&lifetime.lifetime)
                } else {
                    None
                }
            })
            .collect();

        let lifetime_quote = if lifetimes.is_empty() {
            quote! {}
        } else {
            quote! { <#(#lifetimes),*>}
        };

        let gen = quote! {
            impl<'a> CloneIn<'a> for #name #lifetime_quote {
                fn clone_in(&self, arena:&'a Bump) -> Self {
                    match self {
                        #(#match_arms),*
                    }
                }
            }
        };

        gen.into()
    } else if let Data::Struct(data) = &ast.data {
        let field_impls = data.fields.iter().map(|field| {
            let field_name = &field.ident;

            quote! {
                #field_name: self.#field_name.clone_in(arena)
            }
        });

        let lifetimes: Vec<&Lifetime> = ast
            .generics
            .params
            .iter()
            .filter_map(|param| {
                if let syn::GenericParam::Lifetime(lifetime) = param {
                    Some(&lifetime.lifetime)
                } else {
                    None
                }
            })
            .collect();

        let lifetime_quote = quote! { <#(#lifetimes),*>};

        let gen = quote! {
            impl<'a> CloneIn<'a> for #name #lifetime_quote {
                fn clone_in(&self, arena: &'a Bump) -> Self {
                    Self {
                        #(#field_impls),*
                    }
                }
            }
        };

        gen.into()
    } else {
        panic!("CloneIn can only be derived for enums or structs");
    }
}
