extern crate proc_macro;
use proc_macro::TokenStream;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, quote, format_ident};
use syn::{Ident, Item, Type, ReturnType, punctuated::Punctuated};

// TODO: could be simpler with `quote!`?
fn box_path(raw: syn::Path) -> syn::TypePath {
    let wrapped_segment = syn::PathSegment {
        ident: raw.segments.first().unwrap().ident.clone(),
        arguments: syn::PathArguments::None
    };

    let mut wrapped_pair = Punctuated::<syn::PathSegment, syn::token::Colon2>::new();
    wrapped_pair.push(wrapped_segment);

    let mut generic_pair = Punctuated::<syn::GenericArgument, syn::token::Comma>::new();
    generic_pair.push(syn::GenericArgument::Type(Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: wrapped_pair
        }
    })));

    let segment = syn::PathSegment {
        ident: Ident::new("Box", raw.segments.first().unwrap().ident.clone().span()),
        arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{
            colon2_token: None,
            lt_token: Default::default(),
            args: generic_pair,
            gt_token: Default::default(),
        })
    };

    let mut new_pairs = Punctuated::<syn::PathSegment, syn::token::Colon2>::new();
    new_pairs.push(segment);

    syn::TypePath{
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: new_pairs,
        }
    }
}

#[proc_macro_attribute]
pub fn capture(_: TokenStream, stream: TokenStream) -> TokenStream {
    let mut item: Item = syn::parse(stream.clone()).unwrap();

    let function = match &mut item {
        Item::Fn(function) => { 
            function 
        },
        _ => { 
            panic!("`#[capture]` only works with functions") 
        }
    };

    let function_name = function.sig.ident.to_string();

    let mut wrapper_name = function_name.clone();
    wrapper_name.insert_str(0, "crossed_ffi_");

    let mut wrapper_function = function.clone();
    wrapper_function.sig.ident = Ident::new(&wrapper_name, Span::call_site());
    wrapper_function.block.stmts = vec![];

    let public_token: syn::token::Pub = Default::default();

    wrapper_function.vis = syn::Visibility::Public(syn::VisPublic {
        pub_token: public_token
    });

    let extern_token: syn::token::Extern = Default::default();

    wrapper_function.sig.abi = Some(syn::Abi{
        extern_token: extern_token,
        name: Some(syn::LitStr::new("C", Span::call_site()))
    });

    let mut raw_inputs: Vec<String> = Vec::new();
    let mut new_inputs = Punctuated::<syn::FnArg, syn::token::Comma>::new();

    for input in wrapper_function.sig.inputs.iter() {
        if let syn::FnArg::Typed(mut path) = input.clone() {
            if let syn::Pat::Ident(name) = &*path.pat {
                raw_inputs.push(name.ident.to_string());
            }

            if let syn::Type::Path(type_path) = *path.ty.clone() {
                path.ty = Box::new(syn::Type::Path(box_path(type_path.clone().path)));
            }

            new_inputs.push(syn::FnArg::Typed(path));
        }
    }

    wrapper_function.sig.inputs = new_inputs;

    if wrapper_function.sig.output != ReturnType::Default {
        if let ReturnType::Type(arrow, output) = wrapper_function.sig.output.clone() {
            if let Type::Path(path) = &*output {
                wrapper_function.sig.output = ReturnType::Type(arrow, Box::new(Type::Path(box_path(path.clone().path))));
            }
        }
    }

    let mut prefixed_cursors = Vec::new();

    for input in raw_inputs {
        let cursor = format_ident!("{}", input);
        let prefixed_cursor = format_ident!("safe_{}", input);

        wrapper_function.block.stmts.push(syn::parse(
            quote!(
                let #prefixed_cursor = *#cursor;
            ).into()
        ).unwrap());

        prefixed_cursors.push(prefixed_cursor);
    }

    let function_name_ident = format_ident!("{}", function_name);

    if wrapper_function.sig.output == ReturnType::Default {
        wrapper_function.block.stmts.push(syn::parse(quote!(
            #function_name_ident(#(#prefixed_cursors),*);
        ).into()).unwrap());
    } else {
        wrapper_function.block.stmts.push(syn::parse(quote!(
            return Box::new(#function_name_ident(#(#prefixed_cursors),*));
        ).into()).unwrap());
    }

    let mut new_stream: TokenStream2 = item.into_token_stream();

    let ffi_item = Item::Fn(wrapper_function);

    ffi_item.to_tokens(&mut new_stream);

    println!("turned\n  `{}`\ninto\n  `{}`\n", stream.to_string(), new_stream.to_string());

    new_stream.into()
}

#[proc_macro_attribute]
pub fn dump(_: TokenStream, stream: TokenStream) -> TokenStream {
    let item: Item = syn::parse(stream.clone()).unwrap();

    println!("{:#?}", item);

    stream
}
