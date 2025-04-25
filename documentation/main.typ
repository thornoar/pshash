/*
Author: Roman Maksimovich
Library files at https://github.com/thornoar/nixos-config
Asymptote libraries at https://github.com/thornoar/smoothmanifold
*/

// Library imports
#import "@local/common:0.0.0": *
#import "@local/templates:0.0.0": *
#show: article-rule
#import "@local/theorem:0.0.0": *
#show: theorem

// Local definitions
#let concat = math.union.plus
#let elt = math.class("binary", [:])
#let excl = math.class("binary", [!])
#let ch = math.cal([C])
#let mg = math.cal([M])
#let ech = math.overline(math.cal([C]))
#let mc = { mg; h(-2pt); ch }
#let smc = { mg; h(-1pt); ch }
#let hash = math.cal([H])
#let shuf = math.cal([S])
#let oval = math.overline(math.alpha)
#let conf = math.frak([L])
#let lui(n) = $""^#n #h(-1.5pt)$
#let lli(n) = [#move(dy:-2pt, $""_#n$) #h(-1pt)]
#let lun = lui([$n$])
#let lln = lli([$n$])
#let cod = math.op("cod")
#let spr = math.op("spr")
#let rel(n,m) = $(#n#h(.5mm)|#h(.5mm)#m)!$

// Begin Document
#let keywords = ("Combinatorics", "Hash", "Security", "Haskell", "Functional programming")
#show: title-rule([ On rearrangement hashing with Haskell ], abstract: [
  In this paper I introduce and develop a mathematical method of producing a pseudo-cryptographic hash of adjustable length, given a public key and a pair of private keys. The hashing is done through encoding selections and permutations with natural numbers, and then composing the hash from a set of source strings with respect to the permutations encoded by the keys. The attempts to construct a suitable integer-to-selection mapping lead to interesting mathematical definitions and statements, which are discussed in this paper and applied to give bounds on the reliability of the hashing algorithm. An implementation is provided in the Haskell programming language (source available at https://github.com/thornoar/pshash) and applied in the setting of password creation.
], date: datetime(year: 2024, month: 3, day: 26), keywords: keywords, logo: image("figures/logo.svg", width: 60%), keywordlength: 60%)

= Introduction

The motivation behind the topic lies in the management of personal passwords. Nowadays, the average person requires tens of different passwords for different websites and services. Overall, one can distinguish between two ways of managing this set of passwords:

- *Keeping everything in one's head.* This is a method employed by many, yet it inevitably leads to certain risks. First of all, in order to fit the passwords in memory, one will probably make them similar to each other, or at least have them follow a simple pattern like "[shortened name of website]+[fixed phrase]". As a result, if even one password is guessed or leaked, it will be almost trivial to retrieve most of the others, following the pattern. Furthermore, the passwords themselves will tend to be memorable and connected to one's personal life, which will make them easier to guess. There is, after all, a limit to one's imagination.

- *Storing the passwords in a secure location.* Arguably, this is a better method, but there is a natural risk of this location being revealed, or of the passwords being lost, especially if they are stored physically on a piece of paper. Currently, various "password managers" are available, which are software programs that will create and store your passwords for you. It is usually unclear, however, how this software works and whether it can be trusted with one's potentially very sensitive passwords. After all, guessing the password to the password manager is enough to have all the other passwords exposed.

In this paper I suggest a way of doing neither of these things. The user will not know the passwords or have any connection to them whatsoever, and at the same time the passwords will not be stored anywhere, physically or digitally. In this system, every password is a cryptographic hash produced by a fixed hashing algorithm. The algorithm requires three inputs: one public key, i.e. the name of the website or service, and two private keys, which are arbitrary positive integers known only to the user (the initial version of the algorithm, described in later sections, will only use one private key). Every time when retrieving a password, the user will invoke the keys to re-create it from scratch. Therefore, in order to be reliable, the algorithm must be "pure", i.e. must always return the same output given the same input. Additionally, the algorithm must be robust enough so that, even if a hacker had full access to it and its mechanics, they would still not be able to guess the user's private key or the passwords that it produces. These considerations naturally lead to exploring pure mathematical functions as hashing algorithms and implementing them in a functional programming language such as Haskell.

= The theory

There are many ways to generate hash strings. In our case, these strings are potential passwords, meaning they should contain lower-case and upper-case letters, as well as numbers and special characters. Instead of somehow deriving such symbol sequences directly from the public and private keys, we will be creating the strings by selecting them from a pre-defined set of distinct elements (i.e. the English alphabet or the digits from 0 to 9) and rearranging them. The keys will play a role in determining the rearrangement scheme. With regard to this strategy, some preliminary definitions are in order.

== Preliminary terminology and notation

Symbols $A$, $B$, $C$ will denote arbitrary sets (unless specified otherwise). $NN_0$ is the set of all non-negative integers.

By $E$ we will commonly understand a finite enumerated set of distinct elements, called a _source._ When multiple sources $E_0$, $E_1$, ..., $E_(N-1)$ are considered, we take none of them to share any elements between each other. In other words, their pair-wise intersections will be assumed to be empty.

The symbol "$\# $" will be used to describe the number of ways to make a combinatorial selection. For example, $\#^m (E)$ is the number of ways to choose $m$ elements from a source $E$ with significant order.

The expression $[A]$ will denote the set of all ordered lists composed from elements of the set $A$. We assume that all elements in a list are distinct. Every list can therefore be considered a source. The subset $[A]_m subset [A]$ will include only the lists of length $m$. Extending the notation, we will define $[A_0, A_1, ..., A_(N-1)]$ as the set of lists $alpha = [a_0, a_1, ..., a_(N-1)]$ of length $N$ where the first element is from $A_0$, the second from $A_1$, and so on, until the last one from $A_(N-1)$. Finally, if $alpha in [A]$ and $beta in [B]$, the list $alpha concat beta in [A union B]$ will be the concatenation of lists $alpha$ and $beta$.

Let $alpha$ be a list. $|alpha|$ will denote its length, while $alpha elt i$ will represent its $i$-th element, with the enumeration starting from $i = 0$. By contrast, the expression $alpha excl i$ will denote the list $alpha$ without its $i$-th element. All sources are associated with the ordered list of all their elements, and thus expressions such as $E elt i$ and $|E|$ have meaning for a source $E$.

Let $k in NN_0$, $n in NN$. The numbers $lun k, lln k in NN_0$ are defined to be such that $0 <= lun k < n$ and\ $lln k dot n + lun k = k$. The number $lun k$ is the remainder after division by $n$, and $lln k$ is the result of division.

For a number $n in NN$, the expression $(n)$ will represent the semi-open integer interval from 0 to $n$: $(n) = \{0, 1, ..., n-1\}$.

Let $n, m in NN$, $m <= n$. The quantity $n!\/(n-m)!$ will be called a _relative factorial_ and denoted by $rel(n,m)$#h(3pt).

Consider a function $f$ of many arguments $a_0$, $a_1$, ..., $a_(n-1)$. Then with the expression $f(a_0, ..., a_(i-1), -, a_(i+1), ..., a_(n-1))$ we will denote the function of one argument $a_i$ where all others are held constant.

== Enumerating list selections

The defining feature of the public key is that it is either publicly known or at least very easy to guess. Therefore, it should play little role in actually encrypting the information stored in the private key. It exists solely for the purpose of producing different passwords with the same private key. So for now we will forget about it. In this and the following subsection we will focus on the method of mapping a private key $k in NN_0$ to an ordered selection from a set of sources in an effective and reliable way.

#def("First-order choice function")[
  Let $E$ be a source, $k in NN_0$. The _choice function of order 1_ is defined as the following one-element list:
  $
    ch^1(E, k) = [E elt lui(|E|) k].
  $
] <c1ek>

It corresponds to picking one element from the source according to the key. For a fixed source $E$, the choice function is periodic with a period of $|E|$ and is injective  on the interval $(|E|)$ with respect to $k$. Injectivity is a very important property for a hash function, since it determines the number of keys that produce different outputs. When describing injectivity on intervals, the following definition proves useful:

#def[
  Let $A$ be a finite set and let $f : NN_0 -> A$ be a function. The _spread_ of $f$ is defined to be the largest number $n$ such that, for all $k_1, k_2 in NN_0$, $k_1 != k_2$, the following implication holds:
  $
    f(k_1) = f(k_2) ==> |k_1 - k_2| >= n.
  $
  This number exists due to $A$ being finite. We will denote this number by $#spr (f)$.
]

Trivially, if $#spr (f) >= n$, then $f$ is injective on $(n)$, but the inverse is not always true. Therefore, a lower bound on the spread of a function serves as a guarantee of its injectivity. Furthermore, if $#spr (f) >= n$ and $f$ is bijective on $(n)$, then $f$ is periodic with period $n$ and therefore has a spread of exactly $n$. We leave this as a simple exercise for the reader.

#prop[
  Let $f : NN_0 -> A$, $g : NN_0 -> B$ be functions such that $#spr (f) >= n$ and $#spr (g) >= m$. Define the function $h : NN_0 -> [A, B]$ as follows:
  $
    h(k) = [f(lun k),#h(5pt) g(lln k + T(lun k))],
  $
  where $T : NN_0 -> NN_0$ is a fixed function, referred to as the *argument shift function.* It is then stated that $#spr (h) >= n m$.
] <map>
#pf[
  Assume that $k_1 != k_2$ and $h(k_1) = h(k_2)$. Since $h$ returns an ordered list, the equality of lists is equivalent to the equality of all their corresponding elements:
  $
    f(lun k_1) &= f(lun k_2),\
    g(lln k_1 + T(lun k_1)) &= g(lln k_2 + T(lun k_2)).
  $
  Since $f$ is injective on $(n)$, we see that $lun k_1 = lun k_2$. Consequently, it follows from $k_1 != k_2$ that $lln k_1 != lln k_2$ and $lln k_1 + T(lun k_1) != lln k_2 + T(lun k_2)$. We then utilize the definition of $spr(g)$:
  $
    abs(lln k_1 + T(lun k_1) - lln k_2 + T(lun k_2)) &>= m,\
    #vphantom(1em)
    abs(lln k_1 - lln k_2) &>= m,\
    abs((k_1 - lun k_1)/n - (k_2 - lun k_2)/n) &>= m,\
    abs((k_1 - k_2)/n) &>= m,\
    abs(k_1 - k_2) &>= n m,
  $
  q.e.d.
]

With this proposition at hand, we have a natural way of extending the definition of the choice function:

#def[
  Let $E$ be a source with cardinality $|E| = n$, $k in NN_0$, $2 <= m <= n$. The _choice function of order $m$_ is defined recursively as
  $
    ch^m (E, k) = [E elt lun k] concat ch^(m-1) (E', k'),
  $
  where $E' = E excl lun k$ and $k' = lln k + T(lun k)$, while $T : NN_0 -> NN_0$ is a fixed argument shift function.
]

#prop[
  Let $E$ be a source with cardinality $n$. Then the choice function $ch^m (E, k)$ of order $m <= n$, as a function of $k$, has a spread of at least $rel(n,m)$#h(1pt) .
]
#pf[
  We will conduct a proof by induction over $m$. In the base case, $m = 1$, we notice that $rel(n,m) = n$, and the statement trivially follows from the definition of $ch^1 (E,k)$.

  Let us assume that the statement is proven for all choice functions of order $m - 1$. Under closer inspection it is clear that the definition of $ch^m (E,k)$ follows the scheme given in @map, with $ch^1 (E,k)$ standing for $f$ and $ch^(m-1)(E', k')$ standing for $g$. The application of the proposition is not straightforward, and we encourage the reader to consider the caveats. Thus, we can utilize the statement of the proposition as follows:
  $
    #spr (ch^m (E,k)) >= #spr (ch^1 (E, -)) dot #spr (ch^(m-1) (E', -)) >=\ >= n dot rel((n-1), (m-1)) = rel(n,m),
  $
  q.e.d.
]

The preceding result is especially valuable considering the fact that there are exactly $rel(n,m)$ ways to select an ordered sub-list from a list, meaning that $ch^m (E, k)$ is not only injective, but also surjective with respect to $k$ on the interval $(rel(n,m))$. This makes it a bijection
$
  ch^m (E, -) : (rel(n,m)) -> [E]_m,
$
and therefore a periodic function with a spread of exactly $rel(n,m) =: \#^m (E)$.

These properties make the choice function a fine candidate for a hash mapping. Suppose that the source $E$ is composed from lower-case and upper-case Latin characters, as well as special symbols and digits:

#align(center)[
  $E = #[`qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0123456789!@#$%`]$ //$
]

The choice function gives us a way to enumerate all possible ways to select a sub-list from $E$. What is more, these selections can be made more "random" and unpredictable by means of complicating the argument shift function $T$. A reasonable practice is to set $T(lui(n)k)$ to the ASCII value of the character $E elt lui(n)k$. This way, each chosen character will influence the choice of the next, creating what is called a "chaotic system", where its behavior is fully determined, but even small changes to inputs eventually produce large changes in the output. Here is a little input-output table for the choice function of order 10 with the specified source and shift function:

#align(center)[
  #table(
    columns: (15%, 20%),
    table.header([*input*], [*output*]),
    align: horizon,
    inset: 7pt,
    stroke: none,
    table.hline(),
    [123], table.vline(), [`41BeGs9$Dd`], //$
    [124], [`52NgJfZIk7`],
    [125], [`63MfHs9$Da`], //$
    [126], [`740VbDo6@u`],
    [127], [`851Br469$S`] //$
  )
]

There is, however, a serious problem. This selection method does not guarantee that the chosen 10 symbols will contain lower-case and upper-case characters, as well as digits and spacial symbols, all at the same time. Since the choice function is bijective, there is a key that produces the combination `"djaktpsnei"`, which will not be accepted as a password in many places, because it contains only one category of symbols. Fortunately, there is a solution.

== Elevating the choice function

#def[
  Let $conf$ be be a list of pairs $(E_i, m_i)$, where $E_i$ are sources, $|E_i| = n_i$,\ $m_i <= n_i$, for $i in (N)$. The _elevated choice function_ corresponding to these data is defined for a key $k in NN_0$ by means of the following recursion:
  $
    ech (conf, k) = [ ch^(m_0)(E_0, lui(n_0) k) ] concat ech (conf excl 0, #h(5pt) lli(n_0) k + T(lui(n_0) k)),
  $
  where $T$ is an argument shift function. The base of the recursion is given when $conf$ is empty, in which case $ech ([#hphantom(1pt)], k) = [#hphantom(1pt)]$. Otherwise, for every key $k$, its image is an element of
  $
    cod (ech (conf, -)) = [[E_0]_(m_0), [E_1]_(m_1), ..., [E_N]_(m_N)].
  $
  In this context, the list $conf$ will be called a _source configuration._
]

In other words, the elevated choice function is a "mapping" of the choice function over a list of sources, it selects a sub-list from every source and then composes the results in a list, which we call a _multiselection._ A trivial application of @map shows that the spread of $ech (conf, -)$ is at least
$
  product_(i = 0)^(N-1) #spr (ch^(m_i) (E_i, -)) = product_(i = 0)^(N-1) rel(n_i,m_i),
$ <elspr>
where $E_i$, $n_i$, and $m_i$ compose the configuration $conf$. In fact, due to the rule of product in combinatorics, we see that the expression in @elspr directly corresponds to the number of possible multiselections from $conf$, or $\#^(ech) (conf)$ for short. Therefore, $ech (conf, -)$ is bijective on the interval $(\#^(ech) (conf))$ and periodic with period $\#^(ech) (conf)$.

This solves the problem with lacking symbol categories --- now we can separate upper-case letters, lower-case letters, numbers, etc., into different sources and apply the elevated choice function, specifying the number of symbols from each source. However, there are two issues arising:

- The result of the elevated choice function will be something like `"amwYXT28@!"`, which is not a bad password, but it would be nice to be able to shuffle the individual selections between each other instead of lining them up one after another.
- Despite the fact that the argument shift function makes the password selection chaotic, the function is a bijection, which means that it can be reversed. With sufficient knowledge of the algorithm, a hacker can write an inverse algorithm that retrieves the private key from the resulting password. This is a deal breaker for our function, because it defeats the purpose --- you may as well have one password for everything. The way to solve this problem it to make the choice function artificially non-injective, or non-collision-free, in a controlled way. In such case, many different keys will produce the same password, and it will be impossible to know which one of them is the correct one. This violates the common non-collision property of hash functions, but it is necessary given the nature of the function we are developing.

We will solve one problem at a time.

== The merge function

#prop[
  Let $f : A times NN_0 -> B$ and $g : B times NN_0 -> C$ be functions such that\ $#spr (f) >= n$ and $#spr (g) >= m$, where the spread is taken with respect to the second argument. Assume also that $g$ is *absolutely injective* with respect to the first argument, that is,
  $
    forall (b_1, k_1), (b_2, k_2) in B times NN_0:  hs hs g(b_1, k_1) = g(b_2, k_2) ==> b_1 = b_2.
  $
  Define the function $h : A times NN_0 -> C$ by
  $
    h(a,k) = g(f(a, lun k), hs lln k + T(lun k)),
  $
  where $T$ is an argument shift function. It is stated that $#spr (h) >= n m$ with respect to $k$.
] <comp>
#pf[
  Let an element $a in A$ and let $k_1, k_2$ be distinct numbers such that $h(a, k_1) = h(a, k_2)$. That implies,
  $
    g(f(a, lun k_1), hs lln k_1 + T(lun k_1)) = g(f(a, lun k_2),#h(5pt) lln k_2 + T(lun k_2)).
  $
  Since $g$ is absolutely injective, we see that $f(a, k_1) = f(a, k_2)$, which means that\ $lun k_1 = lun k_2$, since $f$ is injective on $(n)$. Now, since the first argument of $g$ in the above equation is the same, we can use the definition of spread for the function $g$:
  $
    abs(lln k_1 + T(lun k_1) - lln k_2 - T(lun k_2)) &>= m,\
    #vphantom(1em)
    abs(lln k_1 - lln k_2) &>= m,\
    abs((k_1 - lun k_1)/n - (k_2 - lun k_2)/n) &>= m,\
    abs((k_1 - k_2)/n) &>= m,\
    abs(k_1 - k_2) &>= n m,
  $
  q.e.d.
]

#def[
  Let $E_1$, $E_2$ be two sources, $m_1$, $m_2$ be numbers such that $m_1 <= abs(E_1)$ and $m_2 <= abs(E_2)$. Define the _merge function of order 2,_
  $
    mg^2 : [E_1]_(m_1) times [E_2]_(m_2) times NN_0 -> [E_1 union E_2]_(m_1 + m_2),
  $
  with the following recursive procedure: for $alpha in [E_1]_(m_1), hs beta in [E_2]_(m_2), hs k in NN_0$ consider two cases:
  + Either $alpha$ or $beta$ is empty, that is, $m_1 = 0$ or $m_2 = 0$.
    Then set $mg^2 (alpha, beta, k)$ to be equal to $alpha concat beta$.
  + Neither $alpha$ nor $beta$ is empty.
    Then we will assume that the merge function is already defined for $(alpha excl 0, beta, -)$ and $(alpha, beta excl 0, -)$. Let $s_1$ be the spread of the function $mg^2 (alpha excl 0, beta, -)$ and $s_2$ be the spread of $mg^2 (alpha, beta excl 0, -)$. Finally, denote the remainder $lui((s_1 + s_2)) k$ by $k'$. The merge of $alpha$ and $beta$ with key $k$ and an argument shift function $T$ is defined as
    $
      mg^2 (alpha, beta, k) = cases(
        [alpha elt 0] concat mg^2 (alpha excl 0, hs beta, hs k' + T(k'))\, #h(0.7em) k' < s_1,
        [beta elt 0] concat mg^2 (alpha, hs beta excl 0, hs k' + T(k'))\, #h(0.7em) #mtxt("otherwise,") 
      )
    $
    // where $T$ is an argument shift function.
]

The merge function takes two lists and combines them together in one, in such a way that the order of elements in each of the two lists is not disturbed. For example, the merge of $[1,2,3]$ and $[a,b,c]$ with a certain key could be $[1,a,b,2,c,3]$. We will now derive some properties of $mg^2$. If $s_1$ and $s_2$ are what they are in the above definition, we immediately see that $mg^2(alpha, beta, -)$ is periodic with period $s_1 + s_2$, since it depends only on $k' = #lui($(s_1 + s_2)$)k$. Moreover, it is clear from the definition that $mg^2(alpha, beta, -)$ is injective on the interval $((s_1 + s_2))$, which means that its spread is equal exactly to $s_1 + s_2$:
$
#spr (mg^2 (alpha, beta, -)) = #spr (mg^2 (alpha excl 0, beta, -)) + #spr (mg^2 (alpha, beta excl 0, -)).
$ <recspr>
// From this recursive relationship we can derive this useful proposition:

#prop[
  Let $alpha$ and $beta$ be elements of $[E_1]_(m_1)$ and $[E_2]_(m_2)$ respectively. Then the spread of the corresponding merge function, with respect to the key $k$, is equal to $(m_1 + m_2)!/(m_1 ! dot m_2 !)$.
]
#pf[
  We will conduct a proof by induction over the sum $m_1 + m_2$. The base case is provided by the situation where either $m_1$ or $m_2$ is zero. Now assume that $m_1, m_2 != 0$ and for all similar pairs with sum $m_1 + m_2 - 1$ the statement is proven. Recall @recspr, which we can now transform due to the induction hypothesis:
  $
    spr (mg^2 (alpha, beta, -)) = ((m_1 - 1) + m_2)!/((m_1 - 1)! dot m_2 !) + (m_1 + (m_2 - 1))!/(m_1 ! dot (m_2 - 1)!) =\
    #vphantom(25pt)
    = (m_1 dot (m_1 + m_2 - 1)! + m_2 dot (m_1 + m_2 - 1)!)/(m_1 ! dot m_2 !) = (m_1 + m_2)!/(m_1 ! dot m_2 !),
  $
  q.e.d.
]

Using some combinatorial logic, we can see that the number $(m_1 + m_2)!/(m_1 ! dot m_2 !)$ corresponds to the number of all possible ways to merge the lists $alpha$ and $beta$, which we will denote by $\#^mg (alpha, beta)$. It means that the function $mg^2 (alpha, beta, -)$ is in fact surjective, and therefore bijective, on the interval from zero to its spread. Now we will, as usual, expand its definition beyond only two lists.

#def[
  Let $oval = [alpha_0, alpha_1, ..., alpha_(N-1)]$ be a list of lists, where $N >= 2$ and $a_i in [E_i]_(m_i)$. Define the _merge function of order $N$_ recursively as follows:
  $
    mg^N (oval, k) = cases(
      mg^2 (alpha_0, alpha_1, k)\, #h(1em) N = 2\,,
      mg^2(alpha_0, hs mg^(N-1)(oval excl 0, lui(s) k), hs lli(s) k + T(lui(s) k))\, #h(1em) N > 2\,
    )
  $
  where $s$ is the spread of the function $mg^(N-1)(oval excl 0, hs -)$, and $T$ is an argument shift function.
]

#exer[
  Using induction and @comp, prove the following result for the spread of the merge function:
  $
    spr (mg^N (oval, -)) = (sum_(i=0)^(N-1) m_i)!/(product_(i=0)^(N-1) m_i !),
  $
  where $oval = [alpha_0, alpha_1, ..., alpha_(N-1)] in [[E_0]_(m_0), [E_1]_(m_1), ..., [E_(N-1)]_(m_(N-1))]$.
]

== Merging the multiselection

#def[
  Let $conf$ be a source configuration of pairs $(E_i, m_i)$ for $i in (N)$, and let $k$ be a non-negative integer. We define the _merged choice function,_ corresponding to $conf$, as follows:
  $
    mc (conf, k) = mg^N (ech (conf, lun k), hs lln k + T(lun k)),
  $
  where $n$ is the spread of the elevated choice function $ech (conf, -)$, while $T$ is a fixed argument shift function.
]

In other words, the merged choice function selects $N$ lists from the configuration via $ech$, and then merges them together using $mg^N$. We have therefore solved our first problem --- the password resulting from this new hash function will have different categories of characters mixed together. What is more important, there is no information lost in the process, which is illustrated by the following proposition:

#prop[
  Let $conf$ be a source configuration of length $N$. The following lower bound takes place for the spread of the merged choice function:
  $
    spr (mc (conf, -)) >= spr (ech (conf, -)) dot spr (mg^N (oval, -)) = product_(i=0)^(N-1) rel(n_i,m_i) dot (sum_(i=0)^(N-1) m_i)!/(product_(i=0)^(N-1) m_i !),
  $ <sprmc>
  where $oval$ is the multiselection arising from the application of $ech$, while $n_i$ are the lengths of sources $E_i$ in the configuration $conf$.
]
#pf[
  Since the merge function $mg^N$ preserves the order of the lists it merges, we can see that it is absolutely injective with respect to its first argument, $oval$. Indeed, no matter how the multiselection $oval$ is merged, all elements of, say, $E_0$, can be read from the resulting merged list in the order that they were originally. In other words, the list $alpha_0 in oval$ can be reconstructed from the output of $mg^N (oval, k)$ for all $k$. And so can $alpha_1$, $alpha_2$, etc. Here we are actively using the fact that all elements across all sources $E_0$, $E_1$, ..., $E_(N-1)$ are distinct. Now that the absolute injectivity of $M^N$ has been established, the present statement immediately follows from an application of @comp.
]

It can once again be shown using combinatorics, that the final expression in @sprmc is, in fact, the total number of ways to choose a multiselection from $conf$ and merge it (the reader should not hesitate to check this). Therefore, we conclude that the function $mc (conf, -)$ is bijective on the interval from zero to its spread, and therefore periodic with the period of its spread.

== Double encryption

However, we still have one problem left: our current hash function is bijective, and it can be reverse-engineered relatively easily to retrieve the private key from the final hash. To prevent this, we will have to make our function a bit less injective --- artificially add inputs that produce the same output, in order to make the function harder to invert.

#def[
  Let $conf$ be a source configuration consisting of pairs $(E_i, m_i)$ for $i in (N)$, where $n_i = abs(E_i)$. Let $k_1, k_2 in NN_0$ be two numbers, referred to as the _primary (or choice) key_ and the _secondary (or shuffle) key._ Define the _hash_ corresponding to these inputs as follows:
  $
    hash (conf, k_1, k_2) = ch^(sum m_i)(mc (conf, k_1), hs k_2).
  $
  This definition can be re-written in a more readable way by defining the _shuffle function_ $shuf (alpha, k)$ for a list $alpha$ as $ch^abs(alpha) (alpha, k)$ and letting the source configuration $conf$ be the varying argument:
  $
    hash (-, k_1, k_2) = shuf(-, k_2) circ mc (-, k_1).
  $
  The hash function makes a multiselection from every source in the configuration, then merges them together with the merge function, and finally reshuffles the resulting list.
]

Note that the term "hash" is used loosely here, as it may not adhere to the formal definition of a cryptographic hash. Still, such naming is somewhat justified, given that $hash$ is designed to be a uniformly distributed encryption mapping that is very hard to invert. We will now discuss the properties of $hash (conf, k_1, k_2)$ for a given source configuration $conf$:

- *Injectivity.*
  $hash$ is injective with respect to the choice key $k_1$ on the interval from zero up to
  $
    \#^mc (conf) = spr (mc (conf, -)) = product_(i=0)^(N-1) rel(n_i,m_i) dot (sum_(i=0)^(N-1) m_i)!/(product_(i=0)^(N-1) m_i !).
  $
  This is because $mc (conf, -)$ is injective on this interval, and $shuf (-, k_2)$ is a bijection. With respect to the shuffle key $k_2$, the hash function is injective on the spread of $shuf$, which is
  $
    \#^shuf (conf) = spr (shuf (alpha, -)) = \#^abs(alpha)(alpha) = rel(abs(alpha), abs(alpha)) = abs(alpha)! = (sum_(i=0)^(N-1) m_i)!,
  $
  where $alpha = mc (conf, k_1)$. Therefore, the number of relevant key pairs for the hash function, denoted by $\#^((k_1,k_2))(conf)$, is:
  $
    \#^((k_1,k_2)) = \#^shuf (conf) dot \#^mc (conf) = product_(i=0)^(N-1) rel(n_i,m_i) dot ((sum_(i=0)^(N-1) m_i)!)^2 dot (product_(i=0)^(N-1) m_i !)^(-1) #h(-13pt).
  $ <k1k2>

- *Collisions.*
  However, the number in @k1k2 does not equal the number of all possible values of $hash$. When applying $shuf$ after $mc$, we are changing the order of elements in each source twice. That is, the information about the order of these elements, stored in the output of $ech$, is lost after this output is reshuffled with $shuf$. The number of all possible outputs of $hash$ is the number of ways of choosing $m_i$ elements from $E_i$ (unordered), multiplied by the number of ways to reorder them as one list. We therefore recognize that
  $
    \#^hash (conf) = product_(i=0)^(N-1) mat(n_i; m_i) dot (sum_(i=0)^(N-1) m_i)!
  $
  Now, due to the fact that all resulting hashes are equally likely for $k_1$, $k_2$ within their respective injectivity intervals, we can calculate the number of different $(k_1, k_2)$ pairs that produce the same hash in a fixed configuration, $\#^inter (conf)$:
  $
    \#^inter (conf) = (\#^((k_1,k_2)) (conf))/(\#^hash (conf)) = (product_(i=0)^(N-1) rel(n_i,m_i) dot ((sum_(i=0)^(N-1) m_i)!)^2 dot (product_(i=0)^(N-1) m_i !)^(-1) #h(-13pt))/(product_(i=0)^(N-1) mat(n_i; m_i) dot (sum_(i=0)^(N-1) m_i)!) =\
    #vphantom(25pt)
    = (sum_(i=0)^(N-1) m_i)!
  $
  
With respect to each of the two keys, $hash$ is an injective function, but in combination they clash together and, to a degree, encrypt each other, erasing the trace to the original pair of keys. It does mean that the hash function now requires two private keys instead of one, but it is a minor disadvantage.

#figure(
  image("./figures/hash.svg"),
  caption: [ An illustration of the working of the hash function ]
) <hashfigure>

== Producing different hashes with the same private keys

The premise of this entire discussion was that one requires many passwords for different purposes. Right now, the only way is to create a new primary-secondary key pair for every new occasion. Considering how large these numbers tend to be, you may as well try to remember the hashes themselves. This is where the public key comes into play. The public key, denoted $p$, is an integer number defined by the environment and available to the public. We will assume that it is the name of the website for which we require a password, like `"google"`, for example, converted into a number by treating the characters in the string as digits in base-128. This number acts on the choice private key by shifting it modulo $\#^smc (conf)$:
$
  k'_1 = lui(\#^smc (conf))(k_1 + p),
$
and then this new choice key is plugged into the hash function along with the shuffle key. Due to the injectivity of $hash$ with respect to $k_1$, we see that different public keys produce different output hashes, as long as they remain in the interval $(\#^smc (conf))$. Its influence on $k_1$ is simple and predictable, but it doesn't have to be complex, since the public key is not directly responsible for any encryption. Instead it is designed to be easily remembered.

== Counting ages of the Universe <bignum>

In this subsection, we will use a specific source configuration $conf$, particularly the following:

#figure(
  table(
    columns: 4,
    stroke: none,
    align: (center, left, center, left),
    gutter: 3pt,
    $i$, table.vline(), $E_i$, table.vline(), $n_i$, table.vline(), $m_i$,
    table.hline(),
    [0], `"ckapzfitqdxnwehrolmbyvsujg"`,  [26], [10],
    [1], `"RQLIANBKJYVWPTEMCZSFDOGUHX"`,  [26], [10],
    [2], `"=!*@?$%#&-+^{}"`,              [14], [6],
    [3], `"1952074386"`,                  [10], [6],
  ),
  caption: [ An example of a source configuration ]
)

Therefore, every password produced with this configuration will contain a total of 32 symbols, 10, 10, 6 and 6 from their respective categories.

Now, let's imagine that you have inserted your two private keys into the function and got a password out of it. A sophisticated hacker sets their mind to crack your password whatever it takes. They are very smart and they have a supercomputer that can perform 1,000,000,000,000 password checks in a second, or one picosecond to check one password or key. What is more, they got their hands on the hash function and the configuration you use, so they can try to reverse-engineer your password.

First, they read into the configuration and see the structure of the password. They decide to brute-force it by checking every relevant combination of 32 symbols. Well, they will have to check $\#^hash (conf)$ combinations, which in our case equals exactly
$
  \#^hash (conf) = 4,681,868,099,431,597,288,493,491,203,991,195,290,886,471,680,000,000,000 approx 4 dot 10^54.
$

Cracking it would take the supercomputer about
$
  148,359,447,468,489,290,599,901,768,793,980,928 approx 10^35
  // 45,649,060,759,535,167,757,411,626,837,344,256 
$
years, or about
$
  10,760,821,605,025,697,027,325,952 approx 10^25
$
ages of the Universe.

Okay, thinks the hacker, no luck. They dig a little deeper into the algorithm and find that your password depends on two private keys. The number of all pairs of such keys is
$
  \#^((k_1,k_2)) (conf) =
    1,&231,943,871,416,597,272,872,197,263,679,034,862,\
    &284,237,334,545,743,786,828,619,158,565,735,628,\
    &800,000,000,000,000,000 approx 10^90
    // &379,059,652,743,568,391,652,983,773,439,703,034,\
    // &548,996,102,937,151,934,408,805,894,943,303,270,\
    // &400,000,000,000,000,000 approx 3 times 10^89
$

This one is going to take billions of billions times longer than the previous one.

Okay, thinks the hacker, the night is young. They are able to get their hands on one of your 32-symbol passwords because of a security leak on the website you were registered to. They have also dug ears deep into the hash function and understood how it works to the tiniest detail. Now, they want to retrieve your private keys to be able to generate your passwords to all other websites and services you use. They see that your password starts with a `'W'`. They know it was shuffled from some other position by the shuffle function, $shuf (-, k_2)$, but they don't know what the password was before the shuffling. What they do know is that your password is produced by
$
  \#^inter (conf) = 263,130,836,933,693,530,167,218,012,160,000,000 approx 2 times 10^35
$
different $(k_1, k_2)$ pairs. The hacker can (relatively) easily retrieve any one of these pairs by substituting the intermediary layer $alpha$ (after the application of $mc$, but before $shuf$) of their choosing, and then solve for the two keys. The problem is that if the keys they receive differ from the true keys, they will not produce the correct password given a different public key. Therefore, to get the key to another website, the hacker would need to go through all of $\#^inter (conf)$different combinations, which will take them about $8,338,113,067,333,812 approx 8 times 10^15$ years, or $604,780$ ages of the Universe. It is, of course, almost infinitely better than brute-forcing the password from scratch (after all, the resulting hash does give the hacker a lot of information about the original keys), but it is still pretty much impossible.

After that, the hacker can go to sleep and forget about cracking your password, because they have nothing left to try. And you can go to sleep knowing that your accounts are safe.

== Some advice

All the above calculations are based on the premise that the hacker will not get your key pair among the first couple of hundreds, that they will have to deplete the entire spread of the hash function to find your choice-shuffle pair. Therefore, if your pair is something like $(14, 245)$, the password will not take a single millisecond to be broken. It is therefore advisory that the keys be very large --- somewhere in the middle of the spread. You can practice your memory and remember two large numbers, if you are not afraid of forgetting them (and you should be). Or, you can take the factorial, or a power, of a smaller number and remember that number instead.

But anyway, how can _you_ get some of those hashes yourself?

= The implementation

== A brief introduction to Haskell

Haskell, named after mathematician Haskell Curry, is one of the most renown functional programming languages. The term "functional" here refers to the following principles:

- *Function purity.* While in standard procedural languages a function is usually just a macro, i.e. a piece of code that can be run multiple times, in Haskell and other functional languages functions are understood mathematically. They are abstract and immutable objects that accept inputs and return outputs. They do not have access to any external variables that are not functions themselves. That property is called _purity._

- *Stateless programming.* Ever since the model of Turing machines, the idea of state has been integral to programming. It implies that a program can be in different states, which affects its behavior. This does, however, make the program potentially more unpredictable, since, even given the same input twice, it may return different outputs based on its state. In functional programming, state does not exist. All functions and variables are constant and are not allowed to change. As a result, a Haskell program, much like a mathematical function, is guaranteed to give the same result with the same input.

It is obvious that the hashing algorithm, which is basically a mathematical function and which must return the same hash given the same keys, is best implemented in a functional programming language. Haskell has been chosen because of its simple (yet unconventional) syntax, as well as amazing flexibility and a developed infrastructure for working with numbers and strings. Particularly, it has the ability to store integer numbers of _arbitrary length_ (not just, say, 8 bytes). This is crucial given the monstrous numbers seen in @bignum.

== The `pshash` program

`pshash` (from "password-hash") is the name of the implementation of the hash function described in the previous section. There is no front-end or GUI, it is only a command-line utility designed to be simple and lightweight. The source, written in Haskell, is available at https://github.com/thornoar/pshash. The code repeats the definitions explained in the previous section almost verbatim --- such is the benefit of functional programming.

The program accepts three arguments: the public key and two private keys, returning a hash using its builtin source configuration. On demand, the user may pass a different configuration using one of the `-d`, `-s`, and `-c` command-line options, but this is not recommended, since it will have to be done every time. Instead, it is suggested to download the source, change the default configuration in the code, and then compile it to a new executable. You can run `"pshash --help"` to see all available options.

*! Important notice !*

The program is _extremely sensitive to changes._ Thus, before using it to create passwords, make sure that the configuration is set to your liking, save the executable and do _not_ change it afterwards. Any small change, and all your passwords will be lost. You will have to undo the change to retrieve them. Additionally, it is advised to store the program openly on the internet, in case you lose your local copy. This way, you can access it from anywhere with an internet connection. Currently, there are pre-compiled binaries for Linux (NixOS, Debian, and Arch), as well as Windows and Android, all hosted at https://github.com/thornoar/pshash-bin. There is also a web interface at https://thornoar.github.io/pshash/web/app/.
