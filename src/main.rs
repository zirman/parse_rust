use std::marker::PhantomData;

#[derive(Debug)]
enum ParseResult<'a, A> {
    Parsed(&'a str, A),
    Failed(&'a str),
}

trait Parser<A> {
    fn parse<'a, 'b>(&'a self, source: &'b str) -> ParseResult<'b, A>;
}

trait ParserFunctor<PA, FAB, A, B>
where
    PA: Parser<A>,
    FAB: Fn(A) -> B,
{
    fn map(self, f: FAB) -> ParserMap<PA, FAB, A, B>;
}

trait ParserMonad<PA, FAPB, PB, A, B>
where
    PA: Parser<A>,
    FAPB: Fn(A) -> PB,
    PB: Parser<B>,
{
    fn bind(self, f: FAPB) -> ParserBind<PA, FAPB, PB, A, B>;
}

struct ParserMap<PA, FAB, A, B>(PA, FAB, PhantomData<A>)
where
    PA: Parser<A>,
    FAB: Fn(A) -> B;

impl<PA, FAB, A, B> Parser<B> for ParserMap<PA, FAB, A, B>
where
    PA: Parser<A>,
    FAB: Fn(A) -> B,
{
    fn parse<'a, 'b>(&'a self, source: &'b str) -> ParseResult<'b, B> {
        let ParserMap(pa, fab, _) = self;

        match pa.parse(source) {
            ParseResult::Parsed(source1, x) => ParseResult::Parsed(source1, fab(x)),
            ParseResult::Failed(source1) => ParseResult::Failed(source1),
        }
    }
}

impl<PA, FAB, FBC, A, B, C> ParserFunctor<ParserMap<PA, FAB, A, B>, FBC, B, C>
    for ParserMap<PA, FAB, A, B>
where
    PA: Parser<A>,
    FAB: Fn(A) -> B,
    FBC: Fn(B) -> C,
{
    fn map(self, f: FBC) -> ParserMap<ParserMap<PA, FAB, A, B>, FBC, B, C> {
        ParserMap(self, f, PhantomData)
    }
}

impl<PA, FAB, FBPC, PC, A, B, C> ParserMonad<ParserMap<PA, FAB, A, B>, FBPC, PC, B, C>
    for ParserMap<PA, FAB, A, B>
where
    PA: Parser<A>,
    FAB: Fn(A) -> B,
    FBPC: Fn(B) -> PC,
    PC: Parser<C>,
{
    fn bind(self, f: FBPC) -> ParserBind<ParserMap<PA, FAB, A, B>, FBPC, PC, B, C> {
        ParserBind(self, f, PhantomData, PhantomData, PhantomData)
    }
}

struct ParserBind<PA, FAPB, PB, A, B>(PA, FAPB, PhantomData<PB>, PhantomData<A>, PhantomData<B>)
where
    PA: Parser<A>,
    FAPB: Fn(A) -> PB,
    PB: Parser<B>;

impl<PA, FAPB, PB, A, B> Parser<B> for ParserBind<PA, FAPB, PB, A, B>
where
    PA: Parser<A>,
    FAPB: Fn(A) -> PB,
    PB: Parser<B>,
{
    fn parse<'a, 'b>(&'a self, source: &'b str) -> ParseResult<'b, B> {
        let ParserBind(pa, fapb, _, _, _) = self;

        match pa.parse(source) {
            ParseResult::Parsed(source1, x) => fapb(x).parse(source1),
            ParseResult::Failed(x) => ParseResult::Failed(x),
        }
    }
}

impl<PA, FAPB, PB, FBC, A, B, C> ParserFunctor<ParserBind<PA, FAPB, PB, A, B>, FBC, B, C>
    for ParserBind<PA, FAPB, PB, A, B>
where
    PA: Parser<A>,
    FAPB: Fn(A) -> PB,
    PB: Parser<B>,
    FBC: Fn(B) -> C,
{
    fn map(self, f: FBC) -> ParserMap<ParserBind<PA, FAPB, PB, A, B>, FBC, B, C> {
        ParserMap(self, f, PhantomData)
    }
}

impl<PA, FAPB, PB, FBPC, PC, A, B, C> ParserMonad<ParserBind<PA, FAPB, PB, A, B>, FBPC, PC, B, C>
    for ParserBind<PA, FAPB, PB, A, B>
where
    PA: Parser<A>,
    FAPB: Fn(A) -> PB,
    PB: Parser<B>,
    FBPC: Fn(B) -> PC,
    PC: Parser<C>,
{
    fn bind(self, f: FBPC) -> ParserBind<ParserBind<PA, FAPB, PB, A, B>, FBPC, PC, B, C> {
        ParserBind(self, f, PhantomData, PhantomData, PhantomData)
    }
}

struct ParserPure<A>(A)
where
    A: Clone;

impl<A> Parser<A> for ParserPure<A>
where
    A: Clone,
{
    fn parse<'a>(&self, source: &'a str) -> ParseResult<'a, A> {
        let ParserPure(x) = self;
        ParseResult::Parsed(source, x.clone())
    }
}

impl<FAB, A, B> ParserFunctor<ParserPure<A>, FAB, A, B> for ParserPure<A>
where
    FAB: Fn(A) -> B,
    A: Clone,
{
    fn map(self, f: FAB) -> ParserMap<ParserPure<A>, FAB, A, B> {
        ParserMap(self, f, PhantomData)
    }
}

impl<FAPB, PB, A, B> ParserMonad<ParserPure<A>, FAPB, PB, A, B> for ParserPure<A>
where
    FAPB: Fn(A) -> PB,
    PB: Parser<B>,
    A: Clone,
{
    fn bind(self, f: FAPB) -> ParserBind<ParserPure<A>, FAPB, PB, A, B> {
        ParserBind(self, f, PhantomData, PhantomData, PhantomData)
    }
}

struct ParserChar(char);

impl Parser<()> for ParserChar {
    fn parse<'a>(&self, source: &'a str) -> ParseResult<'a, ()> {
        let ParserChar(c1) = self;

        match source.chars().next() {
            Some(c2) => {
                if *c1 == c2 {
                    ParseResult::Parsed(&source[1..], ())
                } else {
                    ParseResult::Failed(source)
                }
            }
            None => ParseResult::Failed(source),
        }
    }
}

impl<FA, A> ParserFunctor<ParserChar, FA, (), A> for ParserChar
where
    FA: Fn(()) -> A,
{
    fn map(self, f: FA) -> ParserMap<ParserChar, FA, (), A> {
        ParserMap(self, f, PhantomData)
    }
}

impl<FPA, PA, A> ParserMonad<ParserChar, FPA, PA, (), A> for ParserChar
where
    FPA: Fn(()) -> PA,
    PA: Parser<A>,
{
    fn bind(self, f: FPA) -> ParserBind<ParserChar, FPA, PA, (), A> {
        ParserBind(self, f, PhantomData, PhantomData, PhantomData)
    }
}

fn main() {
    let abc = ParserChar('a')
        .map(|_| 1)
        .bind(|_| ParserChar('b'))
        .bind(|_| ParserChar('c'))
        .bind(|_| ParserChar('d'))
        .bind(|_| ParserChar('e'))
        .bind(|_| ParserChar('b'))
        .bind(|_| ParserChar('c'))
        .bind(|_| ParserChar('d'))
        .bind(|_| ParserPure(1))
        .map(|x| x + 1)
        .bind(|_| ParserChar('e'))
        .bind(|_| ParserChar('b'))
        .bind(|_| ParserChar('c'))
        .bind(|_| ParserChar('d'))
        .bind(|_| ParserChar('e'))
        .bind(|_| ParserChar('b'))
        .bind(|_| ParserChar('c'))
        .bind(|_| ParserChar('d'))
        .bind(|_| ParserChar('e'));

    match abc.parse("abcdebcdebcdebcdebcdebcdebcdebcdebc") {
        ParseResult::Parsed(_, _) => println!("woot"),
        ParseResult::Failed(_) => println!("poo"),
    };
}
