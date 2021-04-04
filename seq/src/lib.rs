use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Ident, Literal, TokenStream as TStream2, TokenTree};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input, LitInt, Result, Token,
};

struct Seq {
    ident: Ident,
    start: i64,
    end: i64,
    ts: TStream2,
}

impl Seq {
    fn replace_ident(&self, n: i64, ts: TStream2) -> TStream2 {
        let result =
            ts.into_iter()
                .fold((Vec::<TokenTree>::new(), false), |mut acc, tt| match tt {
                    TokenTree::Group(group) => {
                        if acc.1 {
                            unimplemented!()
                        } else {
                            acc.0.push(
                                Group::new(
                                    group.delimiter(),
                                    self.replace_ident(n, group.stream()),
                                )
                                .into(),
                            );
                            acc
                        }
                    }

                    TokenTree::Ident(ref ident) if ident.to_string() == self.ident.to_string() => {
                        if acc.1 {
                            let prev = acc.0.pop().unwrap();
                            acc.0
                                .push(Ident::new(&format!("{}{}", prev, n), prev.span()).into())
                        } else {
                            acc.0.push(Literal::i64_unsuffixed(n).into())
                        }

                        (acc.0, false)
                    }

                    TokenTree::Ident(_) => {
                        if acc.1 {
                            unimplemented!()
                        }
                        acc.0.push(tt);
                        acc
                    }

                    TokenTree::Punct(ref p) if p.to_string() == "#" => {
                        if acc.1 {
                            unimplemented!()
                        }
                        (acc.0, true)
                    }

                    TokenTree::Punct(_) | TokenTree::Literal(_) => {
                        if acc.1 {
                            unimplemented!()
                        }
                        acc.0.push(tt);
                        acc
                    }
                });
        if result.1 {
            unimplemented!()
        }

        result.0.into_iter().collect()
    }

    fn has_loop(&self) -> bool {
        self.check_loop(self.ts.clone())
    }

    fn check_loop(&self, ts: TStream2) -> bool {
        #[derive(PartialEq, Eq)]
        enum State {
            None,
            Pound,
            Parenthesis,
            Complete,
        }

        ts.into_iter().fold(State::None, |state, tt| match tt {
            TokenTree::Group(group) => {
                if state == State::Pound && group.delimiter() == Delimiter::Parenthesis {
                    State::Parenthesis
                } else if self.check_loop(group.stream().clone()) {
                    State::Complete
                } else {
                    State::None
                }
            }
            TokenTree::Ident(_) => State::None,
            TokenTree::Punct(ref p) => match (p.to_string().as_str(), state) {
                ("#", State::None) => State::Pound,
                ("*", State::Parenthesis) => State::Complete,
                _ => State::None,
            },
            TokenTree::Literal(_) => State::None,
        }) == State::Complete
    }

    fn expand_loop(&self, ts: TStream2) -> TStream2 {
        ts.into_iter()
            .fold((None, Vec::<TokenTree>::new()), |mut acc, tt| match tt {
                TokenTree::Group(ref group) => {
                    if acc.0.is_some() && group.delimiter() == Delimiter::Parenthesis {
                        acc.1.extend(
                            (self.start..self.end)
                                .map(|n| self.replace_ident(n, group.stream().clone()))
                                .flatten(),
                        );
                        return acc;
                    }

                    if let Some(prev_tt) = acc.0 {
                        acc.1.push(prev_tt)
                    }
                    acc.1.push(
                        Group::new(group.delimiter(), self.expand_loop(group.stream())).into(),
                    );

                    (None, acc.1)
                }
                TokenTree::Punct(ref p) => {
                    if p.to_string() == "#" {
                        (Some(tt), acc.1)
                    } else if acc.0.is_some() && p.to_string() == "*" {
                        (None, acc.1)
                    } else {
                        acc.1.push(tt);
                        (None, acc.1)
                    }
                }
                TokenTree::Ident(_) | TokenTree::Literal(_) => {
                    acc.1.push(tt);
                    acc
                }
            })
            .1
            .into_iter()
            .collect()
    }
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;

        let inclusive;
        if input.peek(Token![..=]) {
            inclusive = true;
            input.parse::<Token![..=]>()?;
        } else {
            inclusive = false;
            input.parse::<Token![..]>()?;
        }

        let mut end = input.parse::<LitInt>()?.base10_parse()?;
        if inclusive {
            end += 1;
        }

        let content;
        braced!(content in input);

        Ok(Seq {
            ident,
            start,
            end,
            ts: TStream2::parse(&content)?,
        })
    }
}

impl Into<TokenStream> for Seq {
    fn into(self) -> TokenStream {
        if self.has_loop() {
            self.expand_loop(self.ts.clone()).into()
        } else {
            (self.start..self.end)
                .map(|n| self.replace_ident(n, self.ts.clone()))
                .flatten()
                .collect::<TStream2>()
                .into()
        }
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);

    seq.into()
}
