use proc_macro::TokenStream;
use proc_macro2::TokenStream as TStream2;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Data, DeriveInput, Field, Ident, Lit, Meta, NestedMeta, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;

    let builder_name = Ident::new(&format!("{}Builder", name), name.span());

    let fields = match input.data {
        Data::Struct(ref data) => &data.fields,
        Data::Enum(_) | Data::Union(_) => {
            unimplemented!()
        }
    };

    fn get_generic_type<'a>(ty: &'a Type, outer: &'static str) -> Option<&'a Type> {
        match ty {
            Type::Path(path) => {
                if path.qself.is_some() {
                    return None;
                }

                if path.path.leading_colon.is_some() {
                    return None;
                }

                if path.path.segments.len() != 1 {
                    return None;
                }

                let path_seg = path.path.segments.first().unwrap();
                if path_seg.ident.to_string() != outer {
                    return None;
                }

                let ab = match path_seg.arguments {
                    syn::PathArguments::AngleBracketed(ref ab) => ab,
                    syn::PathArguments::None | syn::PathArguments::Parenthesized(_) => return None,
                };

                if ab.args.len() != 1 {
                    return None;
                }

                match ab.args.first().unwrap() {
                    syn::GenericArgument::Type(gty) => Some(gty),
                    syn::GenericArgument::Lifetime(_)
                    | syn::GenericArgument::Binding(_)
                    | syn::GenericArgument::Constraint(_)
                    | syn::GenericArgument::Const(_) => None,
                }
            }
            _ => None,
        }
    }
    fn get_option_type(ty: &Type) -> Option<&Type> {
        return get_generic_type(ty, "Option");
    }
    fn has_builder_attr(field: &Field) -> Result<Option<(Ident, &Type)>, TStream2> {
        for attr in &field.attrs {
            if let Ok(Meta::List(outer_meta)) = attr.parse_meta() {
                if outer_meta.path.to_token_stream().to_string() == "builder" {
                    if outer_meta.nested.len() == 1 {
                        if let NestedMeta::Meta(Meta::NameValue(meta_nv)) =
                            outer_meta.nested.first().unwrap()
                        {
                            if meta_nv.path.to_token_stream().to_string() == "each" {
                                if let Lit::Str(lit) = &meta_nv.lit {
                                    if let Some(ty) = get_generic_type(&field.ty, "Vec") {
                                        return Ok(Some((
                                            Ident::new(&lit.value(), lit.span()),
                                            ty,
                                        )));
                                    }
                                }
                            }
                        }
                    }
                    return Err(syn::Error::new_spanned(
                        outer_meta.to_token_stream(),
                        r#"expected `builder(each = "...")`"#,
                    )
                    .to_compile_error());
                }
            }
        }
        return Ok(None);
    }

    if let Some(err) = fields.iter().find_map(|f| has_builder_attr(f).err()) {
        return err.into();
    }

    let builder_field = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if has_builder_attr(f).unwrap().is_some() || get_option_type(ty).is_some() {
            quote! {
                #name: #ty,
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>,
            }
        }
    });
    let builder_init = fields.iter().map(|f| {
        let name = &f.ident;

        if has_builder_attr(f).unwrap().is_some() {
            quote! {
                #name: std::vec::Vec::new(),
            }
        } else {
            quote! {
                #name: std::option::Option::<_>::None,
            }
        }
    });
    let builder_method = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if let Some((lit, gty)) = has_builder_attr(f).unwrap() {
            quote! {
                fn #lit(&mut self, #name: #gty) -> &mut Self {
                    self.#name.push(#name);
                    self
                }
            }
        } else if let Some(gty) = get_option_type(ty) {
            quote! {
                fn #name(&mut self, #name: #gty) -> &mut Self {
                    self.#name = std::option::Option::<_>::Some(#name);
                    self
                }
            }
        } else {
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::<_>::Some(#name);
                    self
                }
            }
        }
    });
    let builder_build = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if has_builder_attr(f).unwrap().is_some() || get_option_type(ty).is_some() {
            quote! {
                #name: self.#name.clone(),
            }
        } else {
            quote! {
                #name: self.#name.as_ref().ok_or("missing".to_string())?.clone(),
            }
        }
    });

    (quote! {
        pub struct #builder_name {
            #(#builder_field)*
        }

        impl #builder_name {
            #(#builder_method)*

            fn build(&mut self) -> std::result::Result::<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::<_, _>::Ok(#name {
                    #(#builder_build)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_init)*
                }
            }
        }
    }).into()
}
