#![feature(proc_macro_expand)]
extern crate proc_macro;
use proc_macro::TokenStream;

use std::char;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, quote, format_ident};
use syn::{Ident, Item, Type, ReturnType, punctuated::Punctuated};

static VERSION: usize = 1;

enum ComponentType {
    Path,
    Arguments,
    // Outputs
}

enum BaseType {
    Function,
    // Struct,
    // Enum
}

fn box_path(generic: syn::Type) -> syn::TypePath {
    let mut generic_pair = Punctuated::<syn::GenericArgument, syn::token::Comma>::new();
    generic_pair.push(syn::GenericArgument::Type(generic));

    let segment = syn::PathSegment {
        ident: Ident::new("Box", Span::call_site()),
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

fn capture_raw(function: syn::ItemFn, call: TokenStream2, wrapped_self: Option<syn::FnArg>) -> syn::Item {
    let wrapped_name = format!("mike_fn_{}", function.sig.ident.to_string().clone());

    let mut wrapper_function = function.clone();
    wrapper_function.sig.ident = Ident::new(&wrapped_name, Span::call_site());
    wrapper_function.block.stmts = vec![]; 

    let public_token: syn::token::Pub = Default::default();

    wrapper_function.vis = syn::Visibility::Public(syn::VisPublic {
        pub_token: public_token
    });

    let extern_token: syn::token::Extern = Default::default();

    wrapper_function.sig.abi = Some(syn::Abi{
        extern_token,
        name: Some(syn::LitStr::new("C", Span::call_site()))
    });

    let mut raw_inputs: Vec<String> = Vec::new();
    let mut new_inputs = Punctuated::<syn::FnArg, syn::token::Comma>::new();

    for input in wrapper_function.sig.inputs.iter() {
        if let syn::FnArg::Typed(mut path) = input.clone() {
            if let syn::Pat::Ident(name) = &*path.pat {
                raw_inputs.push(name.ident.to_string());
            }

            path.ty = Box::new(syn::Type::Path(box_path(*path.ty)));

            new_inputs.push(syn::FnArg::Typed(path));
        }
    }

    if let Some(unwrapped_self) = wrapped_self {
        wrapper_function.sig.unsafety = Some(Default::default());
        new_inputs.insert(0, unwrapped_self);
    }

    wrapper_function.sig.inputs = new_inputs;

    if wrapper_function.sig.output != ReturnType::Default {
        if let ReturnType::Type(arrow, output) = wrapper_function.sig.output.clone() {
            wrapper_function.sig.output = ReturnType::Type(arrow, Box::new(Type::Path(box_path(*output))));
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

    let statement = {
        if wrapper_function.sig.output == ReturnType::Default {
            quote!(
                #call(#(#prefixed_cursors),*);
            )
        } else {
            quote!(
                return Box::new(#call(#(#prefixed_cursors),*));
            )
        }
    };

    wrapper_function.block.stmts.push(syn::parse(statement.into()).unwrap());

    Item::Fn(wrapper_function)
}

#[proc_macro_attribute]
pub fn capture(_: TokenStream, stream: TokenStream) -> TokenStream {
    let item: Item = syn::parse(stream.clone()).unwrap();

    let function = match item.clone() {
        Item::Fn(function) => { 
            function 
        },
        _ => { 
            panic!("`#[capture]` only works with functions") 
        }
    };

    let call = function.sig.ident.clone();

    let wrapped_item = capture_raw(function.clone(), quote!(#call), None);

    let mut new_stream: TokenStream2 = item.into_token_stream();

    wrapped_item.to_tokens(&mut new_stream);

    println!("turned\n  `{}`\ninto\n  `{}`\n", stream.to_string(), new_stream.to_string());

    new_stream.into()
}

#[proc_macro_attribute]
pub fn expand(_: TokenStream, stream: TokenStream) -> TokenStream {
    let item: Item = syn::parse(stream.clone()).unwrap();

    let implementation = match item.clone() {
        Item::Impl(implementation) => { 
            implementation 
        },
        _ => { 
            panic!("`#[expand]` only works with implementation") 
        }
    };

    let implementation_name = match &*implementation.self_ty {
        syn::Type::Path(type_path) => {
            type_path.path.segments.first().unwrap().ident.clone()
        },
        _ => { panic!("") }
    };

    let mut new_stream: TokenStream2 = item.clone().into_token_stream();

    for item in implementation.items.clone().into_iter() {
        if let syn::ImplItem::Method(mut method) = item {
            let original_call = method.sig.ident.clone();

            method.sig.ident = format_ident!("{}__{}", implementation_name.clone(), method.sig.ident);

            let method_function = syn::ItemFn {
                attrs: method.attrs,
                vis: method.vis,
                sig: method.sig.clone(),
                block: Box::from(method.block)
            };

            let mut segments = Punctuated::<syn::PathSegment, syn::token::Colon2>::new();
            segments.push(syn::PathSegment {
                ident: implementation_name.clone(),
                arguments: syn::PathArguments::None
            });

            let wrapped_self = syn::FnArg::Typed(syn::PatType{
                attrs: Vec::new(),
                pat: Box::new(syn::Pat::Ident(syn::PatIdent {
                    attrs: Vec::new(),
                    by_ref: None,
                    mutability: None,
                    ident: Ident::new("wrapped_self", Span::call_site()),
                    subpat: None
                })),
                colon_token: Default::default(),
                ty: Box::new(syn::Type::Ptr(syn::TypePtr {
                    star_token: Default::default(),
                    const_token: Some(Default::default()),
                    mutability: None,
                    elem: Box::new(syn::Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: segments
                        }
                    }))
                }))
            });

            let captured = {
                if let Some(syn::FnArg::Receiver(_)) = method_function.sig.inputs.first() {
                    capture_raw(method_function, quote!((*wrapped_self).#original_call), Some(wrapped_self))
                } else {
                    capture_raw(method_function, quote!(#implementation_name::#original_call), Some(wrapped_self))
                }
            };

            captured.to_tokens(&mut new_stream);
        }
    }

    println!("turned\n  `{}`\ninto\n  `{}`\n", stream.to_string(), new_stream.to_string());

    new_stream.into()
}

#[proc_macro_attribute]
pub fn dump(_: TokenStream, stream: TokenStream) -> TokenStream {
    let item: Item = syn::parse(stream.clone()).unwrap();

    println!("{:#?}", item);

    stream
}

fn cross_creation_structure(structure: syn::ItemStruct) -> TokenStream2 {
    let structure_ident = structure.ident.clone();
    let function_name = format_ident!("mike_new_{}", structure.ident);

    let function = match structure.fields {
        syn::Fields::Named(named_fields) => {
            let mut names: Vec<Ident> = Vec::new();
            let mut types: Vec<Type> = Vec::new();

            for field in named_fields.named {
                names.push(field.ident.clone().unwrap());
                types.push(field.ty);
            }

            quote!(
                #[allow(non_snake_case)]
                pub extern "C" fn #function_name(#(#names: Box<#types>),*) -> Box<#structure_ident> {
                    return Box::new(#structure_ident {#(#names: *#names),*});
                }
            )
        },
        syn::Fields::Unnamed(fields) => {
            let mut names: Vec<Ident> = Vec::new();
            let mut types: Vec<Type> = Vec::new();

            let mut index: u32 = 0;

            for field in fields.unnamed {
                names.push(format_ident!("{}", char::from_digit(10 + index, 16).unwrap()));
                types.push(field.ty);

                index += 1;
            }

            quote!(
                #[allow(non_snake_case)]
                pub extern "C" fn #function_name(#(#names: Box<#types>),*) -> Box<#structure_ident> {
                    return Box::new(#structure_ident (#(*#names),*));
                }
            )
        },
        _ => { 
            unimplemented!("TODO: add more struct type support") 
        }   
    };

    let item: Item = syn::parse(function.into()).unwrap();
    item.into_token_stream()
}

fn cross_creation_enumerable(enumerable: syn::ItemEnum) -> TokenStream2 {
    let enumerable_type = enumerable.ident.clone();

    let mut new_stream = TokenStream2::new();

    for variant in enumerable.variants.into_iter() {
        let variant_name = variant.ident.clone();
        let function_name = format_ident!("mike_get_{}__{}", enumerable_type, variant_name); 

        //TODO (Linden): merge with struct version, very similar
        let statement = match variant.fields {
            syn::Fields::Unit => {
                quote!(
                    #[allow(non_snake_case)]
                    pub extern "C" fn #function_name() -> Box<#enumerable_type> {
                        Box::new(#enumerable_type::#variant_name)
                    }
                )
            },
            syn::Fields::Unnamed(fields) => {
                let mut names: Vec<Ident> = Vec::new();
                let mut types: Vec<Type> = Vec::new();

                let mut index: u32 = 0;

                for field in fields.unnamed {
                    names.push(format_ident!("{}", char::from_digit(10 + index, 16).unwrap()));
                    types.push(field.ty);

                    index += 1;
                }

                quote!(
                    #[allow(non_snake_case)]
                    pub extern "C" fn #function_name(#(#names: Box<#types>),*) -> Box<#enumerable_type> {
                        Box::new(#enumerable_type::#variant_name(#(*#names),*))
                    }
                )
            },
            syn::Fields::Named(fields) => {
                let mut names: Vec<Ident> = Vec::new();
                let mut types: Vec<Type> = Vec::new();

                for field in fields.named {
                    names.push(field.ident.clone().unwrap());
                    types.push(field.ty);
                }

                quote!(
                    #[allow(non_snake_case)]
                    pub extern "C" fn #function_name(#(#names: Box<#types>),*) -> Box<#enumerable_type> {
                        return Box::new(#enumerable_type::#variant_name{#(#names: *#names),*});
                    }
                )
            }
        };

        statement.to_tokens(&mut new_stream);
    }

    new_stream
}

#[proc_macro_derive(CrossCreation)]
pub fn cross_creation(stream: TokenStream) -> TokenStream {
    let item: Item = syn::parse(stream.clone()).unwrap();

    let new_stream: TokenStream2 = match item {
        Item::Struct(structure) => { 
            cross_creation_structure(structure)
        },
        Item::Enum(enumerable) => {
            cross_creation_enumerable(enumerable)
        },
        _ => { 
            panic!("`#[derive(CrossCreation)]` only works with structures") 
        }
    };

    println!("created wrapper function `{:?}`", new_stream.clone().to_string());

    new_stream.into()
}

macro_rules! expand {($($tt:tt)*) => (
    stringify!($($tt)*).parse::<TokenStream>().expect("failed to expand")
)}

fn unquote(source: String) -> String {
    source.trim_matches('"').to_string()
}

fn length_prefix(source: String) -> String {
    format!("{}{}", source.len(), source)
}

#[proc_macro_attribute]
pub fn export(_: TokenStream, stream: TokenStream) -> TokenStream {
    let item: Item = syn::parse(stream.clone()).unwrap();

    let function = match item {
        Item::Fn(function) =>{ 
            function 
        },
        _ => {
            panic!("#[export] only works with functions")
        }
    };

    let path = unquote(expand!(module_path!()).expand_expr().unwrap().to_string());
    let crate_name = env!("CARGO_CRATE_NAME");
    let function_name = unquote(function.sig.ident.to_string());

    println!("{}::{}::{}", crate_name, path, function_name);

    let delimiter = "_";

    let mut mangled_name = String::new();
    mangled_name.push_str("MIKE");
    mangled_name.push_str(delimiter);

    mangled_name.push_str(&VERSION.to_string());
    mangled_name.push_str(delimiter);

    // Will need to be changed when more types are allowed.
    let base_type = BaseType::Function;
    mangled_name.push_str(&(base_type as usize).to_string());
    mangled_name.push_str(delimiter);

    let path_component = ComponentType::Path;
    mangled_name.push_str(&(path_component as usize).to_string());
    mangled_name.push_str(delimiter);

    let components_in_path: Vec<&str> = path.split("::").collect();
    mangled_name.push_str(&components_in_path.len().to_string());
    mangled_name.push_str(delimiter);

    for component in components_in_path.iter() {
        mangled_name.push_str(&component.len().to_string());
        mangled_name.push_str(delimiter);
        mangled_name.push_str(component);
        mangled_name.push_str(delimiter);
    }

    let arguments_component = ComponentType::Arguments;
    mangled_name.push_str(&(arguments_component as usize).to_string());
    mangled_name.push_str(delimiter);

    let arguments_len = function.sig.inputs.len();
    mangled_name.push_str(&arguments_len.to_string());
    mangled_name.push_str(delimiter);

    // for input in function.sig.inputs.iter() {
    // }

    println!("{}", mangled_name);

    let statement = quote!(
        #function
    );

    let item: Item = syn::parse(statement.into()).unwrap();
    item.into_token_stream().into()
}
