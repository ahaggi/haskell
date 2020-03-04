Gabriel Gonzalez wrote in his blog great information about why you should care, and you truly should care. You can read it [here][1] (and also see [this][2]).

It's about scalability, architecture & design of API. The idea is that there's the "Conventional architecture" that says:

> Combine a several components together of type A to generate a
> "network" or "topology" of type B

The issue with this kind of design is that as your program scales, so does your hell when you refactor.

So you want to change module A to improve your design or domain, so you do. Oh, but now module B & C that depend on A broke. You fix B, great. Now you fix C. Now B broke again, as B also used some of C's functionality. And I can go on with this forever, and if you ever used OOP - so can you.

Then there's what Gabriel calls the "Haskell architecture":

> Combine several components together of type A to generate a new
> component of the same type A, **indistinguishable in character from its substituent parts**

This solves the issue, elegantly too. Basically: do not layer your modules or extend to make specialized ones.<br />
Instead, combine.

So now, what's encouraged is that instead of saying things like "I have multiple X, so let's make a type to represent their union", you say "I have multiple X, so let's combine them into an X". Or in simple English: "Let's make composable types in the very first place." (do you sense the monoids' lurking yet?).

Imagine you want to make a form for your webpage or application, and you have the module "Personal Information Form" that you created because you needed personal information. Later you found that you also need "Change Picture Form" so quickly wrote that. And now you say I want to combine them, so let's make a "Personal Information & Picture Form" module. And in real life scalable applications this can and does get out of hand. Probably not with forms but to demonstrate, you need to compose and compose so you will end up with "Personal Information & Change Picture & Change Password & Change Status & Manage Friends & Manage Wishlist & Change View Settings & Please don't extend me anymore & please & please stop! & STOP!!!!" module. This is not pretty, and you will have to manage this complexity in the API. Oh, and if you want change anything - it probably has dependencies. So.. yeah.. Welcome to hell.

Now let's look at the other option, but first let's look at the benefit because it will guide us to it:

> These abstractions scale **limitlessly** because they always preserve
> combinability, therefore we never need to layer further abstractions
> on top. This is one reason why you should learn Haskell: you learn how
> to build **flat** architectures.

Sounds good, so, instead of making "Personal Information Form" / "Change Picture Form" module, stop and think if we can make anything here composable. Well, we can just make a "Form", right? would be more abstract too.<br />
Then it can make sense to construct one for everything you want, combine them together and get one form just like any other.

And so, you don't get a messy complex tree anymore, because of the key that you take two forms and get one form. So `Form -> Form -> Form`. And as you can already see clearly, this signature is an instance of `mappend`.

The alternative, and the conventional architecture would probably look like `a -> b -> c` and then `c -> d -> e` and then...

Now, with forms it's not so challenging; the challenge is to work with this in real world applications. And to do that simply ask yourself as much as you can (because it pays off, as you can see): How can I make this concept composable? and since monoids are such a simple way to achieve that (we want simple) ask yourself first: How is this concept a monoid?

Sidenote: Thankfully Haskell will very much discourage you to extend types as it is a functional language (no inheritance). But it's still possible to make a type for something, another type for something, and in the third type to have both types as fields. If this is for composition - see if you can avoid it.


  [1]: http://www.haskellforall.com/2014/04/scalable-program-architectures.html
  [2]: http://www.haskellforall.com/2014/07/equational-reasoning-at-scale.html