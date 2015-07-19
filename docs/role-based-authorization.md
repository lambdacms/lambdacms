# Role based authorization

While the README shows how to setup and wire up LambdaCms, this page goes into detail as to how Roles work and why you need a part of it in the Base App.

---

LambdaCms comes with a role-based authorization system. These roles can be granted/revoked on users. When a page is requested by a user, he requests to "perform an action (on ...)". For example, a user wants to visit "/home" (he sends a "GET" request to the "/home" route). In other words, the user wants to perform the "GET" action on the route "/home".

Sometimes we call this "(allowed to) request a route" and sometimes we call this "(allowed to) perform an action". In any case, we mean the same thing: the user does something and we'd like to verify that he is authorized (allowed) to do that thing.

What is also means is that LambdaCms currently doesn't support resource-based authorization. We can allow a user to visit "/article" and with that all 100 articles. We cannot allow a user access to "/article/1" and disallow access to "/article/2".

# Allow

While LambdaCms comes with a role-based authorization system, it doesn't come with pre-defined roles. After all, not every site needs 4 kind of admins or 10 different roles representing some kind of blogger.

What we do get is the `Allow a` data type. It is defined as:

```haskell
data Allow a = Unauthenticated
             | Authenticated
             | Roles a
             | Nobody
```

First lets look at `Unauthenticated`. It is for routes that may be requested by anyone. Simple as that.

 `Authenticated` is for routes which require a user just to be logged-in, no specific role required.

Use `Nobody` for routes that no one as access to. This can be used as a default for any unmatched route. Using Nobody ensures that no one can accidentally access a route that we haven't thought of before. At the same time it allows us to not define each and every route of our webapp, so Yesod won't throw a 500 error at us.

A bit more interesting is `Role a`. This part cannot be defined in Core for the reason mentioned before: it is not and cannot be aware of your specific needs. Lets say we have a website with an Admin, a SuperUser that isn't quite an admin but almost, and a Blogger. We create a type in the Base app to reflect those roles:

```haskell
data RoleName = Admin
              | SuperUser
              | Blogger
              deriving (Eq, Ord, Show, Read, Enum, Bounded)

derivePersistField "RoleName"
```

Two things to note here: 1) The data type is called `RoleName`. We're free to name it anything we want. This name comes back a few times so just remember it. 2) it derives a hand full of things. Lets see why:

- Eq: Simply to match someones role with the required roles.
- Ord: The way Core uses `Role a` is with a Set (`Data.Set`) which requires a type to be Ord and Eq. By using a Set we get some goods:
 - a user can have multiple `RoleName`'s (simply called "Roles" from hereon)
 - a user cannot be, say, Admin multiple times
 - checking if a user is allowed perform an can be implemented by checking the intersection of the required Roles and the ones the user has. A non-empty set means the user is allowed. (this is the default implementation).
- Read: Makes that `RoleName` can be used as type in a Model.
- Show, Enum and Bounded: Core doesn't know what Roles you defined but it still needs to know about them. An example of this is that you can grant/revoke Roles on users in the backend. Core needs to show those Roles on the webpage. This is implemented as: `[minBound .. maxBound]`.

# Tell LambdaCms about Roles

At this point we've seen what LambdaCms gives us (Allow a) and what needs to be done to complete the circle (Roles).  Now it's time to let it know about the roles we've defined (admin, superuser and blogger).

Inside the LambdaCmsAdmin class is a `Roles` type defined as: `type Roles master`.
In our Base app we then say:

```haskell
instance LambdaCmsAdmin App where
    type Roles App = RoleName
```

Now Core knows to use `RoleName` every time it needs to deal with `Roles a` and we can continue with defining "rules".

# Rules

Inside the LambdaCmsAdmin App instance we now define who is allowed to perform what actions. For this, we use the `actionAllowedFor` function.

```haskell
actionAllowedFor :: Route master -> ByteString -> Allow (Set (Roles master))
```
It takes a Route (i.e.: `HomeR`), a request method (i.e.: `GET`) and returns an `Allowed` (i.e.: `Unauthenticated`).

```haskell
actionAllowedFor (HomeR)    "GET" = Unauthenticated
```

Lets create some more exiting rules:

```haskell
-- import qualified Data.Set as S

actionAllowedFor (UserR) "GET"    = Roles $ S.fromList [Admin, SuperUser]
actionAllowedFor (UserR) "PUT"    = Roles $ S.fromList [Admin]
actionAllowedFor (UserR) "DELETE" = Roles $ S.fromList [Admin]
actionAllowedFor (BlogR) _        = Roles $ S.fromList [Admin, Blogger]
actionAllowedFor (MembersR) "GET" = Authenticated
actionAllowedFor _ _              = Nobody
```

This allows admins and superusers to see (GET) user info while restricting deletion (DELETE) and modification (PUT) to admins. Admins and bloggers can perform any action for blogs and any logged-in user can visit the MemberR without the need of a specific role. Any other route is not accessible.

You might note that the rules for PUT and DELETE can be replaced with 1 rule using `_` and that is correct. This is just for illustration purposes.

# Authorization

So far we've seen what `Allow a` is and what needs to be implemented in the Base app to make use of it. We've told LambdaCms about RoleName and we've defined a hand full of rules. Lets verify that a user has sufficient Roles.

```haskell
isAuthorizedTo :: master
               -> Maybe (Set (Roles master))
               -> Allow (Set (Roles master))
               -> AuthResult
```

The `isAuthorizedTo` function needs to know which Roles a user has and which Roles are allowed to perform an action. We use `Maybe` since users don't have to have a role.

> An empty set means the user has insufficient roles, which is different from not having roles at all.

The default implementation of the function is:
*Note that `Maybe (Set (Roles master))` will be `Nothing` when a user is not logged in.*

```haskell
-- import qualified Data.Set as S

isAuthorizedTo _ _           Nobody          = Unauthorized "Access denied."
isAuthorizedTo _ _           Unauthenticated = Authorized
isAuthorizedTo _ (Just _)    Authenticated   = Authorized
isAuthorizedTo _ Nothing     _               = AuthenticationRequired
isAuthorizedTo _ (Just urs) (Roles rrs)    = do
    case (not . S.null $ urs `intersection` rrs) of
        True -> Authorized
        False -> Unauthorized "Access denied."
```

In the same order that is:

- Nobody is authorized
- Everyone is authorzied
- The user is authorized since he needs to be and is authenticated (otherwise it wouldn't match `Just _`)
- Authentication is required. The user is not authenticated but the requested route doesn't match any of the 3 patterns above.
- The user has roles `urs` and the requested action requires roles `rrs`. If an intersection exists, the user is authorized.

---
If you don't want to add "Admin" to every case in "actionAllowedFor", this is the place to add a kind of "catch all":

```haskell
isAuthorizedTo _ (Just urs) (Roles rrs)    = do
    case (Admin `S.member` urs) of
        True -> Authorized
        False ->
            case (not . S.null $ urs `intersection` rrs) of
                True -> Authorized
                False -> Unauthorized "Access denied."
```
---

Then spark it off in `instance Yesod App`:

```haskell
isAuthorized theRoute _ = do
    mauthId <- maybeAuthId
    wai <- waiRequest
    y <- getYesod
    murs <- fmapMaybeM getUserRoles mauthId
    return $ isAuthorizedTo y murs $ actionAllowedFor theRoute (W.requestMethod wai)
```
