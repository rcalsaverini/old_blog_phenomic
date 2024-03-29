---
title: Haskell architectural choices?
author: Rafael S. Calsaverini
tags: typeclasses, free monads, architecture, haskell
---

## Motivation

I've been playing on and off again with the [Haskell programming language][haskell] for more than six years now, but I never ever sat down to try to build real software with it. I've played around, read a lot about it, read a lot about the theoretical concepts around it and even used it a bit in production with very simple, localized, script-like small chunks of code that did very specialized things.

After all this time I think I have enough time and will to actually try to do something more complex in Haskell. The problem is: I've spent most of my time around haskell learning theoretical concepts, syntax and curiosities. I didn't spent any time learning design patterns and software engineering in Haskell. And I really think learning about those things isn't possible until you start to write an actual application.


## Clean Architecture

When developing applications in typical object-oriented fashion, I typically resort to the so called [clean architecture][clean-arch] idea:

![][clean-arch-img]

This principle consists mainly in making sure dependencies always flow from code inwards in the figure above. At the very core your main domain entities - typically, in Java or similar languages, the classes that define the domain where your applications works and the basic logic of those objects. Things like: `User`, `CheckingAccount`, ...

In the next layer are Use Cases - classes that define the logic of your application. If your application have users and they can create accounts, you would have a use case `CreateNewUserAccount`, describing the logic of how a user create a new account. The important part is that this code will **only depend on the domain entities**.



[haskell]: https://www.haskell.org/
[clean-arch]: https://8thlight.com/blog/uncle-bob/2012/08/13/the-clean-architecture.html
[clean-arch-img]: https://8thlight.com/blog/assets/posts/2012-08-13-the-clean-architecture/CleanArchitecture-8b00a9d7e2543fa9ca76b81b05066629.jpg
