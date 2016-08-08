#servant-auth-hmac

##Description
Authentication via hashed message code (HMAC) based on [RFC1945](https://tools.ietf.org/html/rfc1945#section-11).

##Status
The library is under development.

At the moment here is a working prototype.
Further development will proceed.


##Demo
The example consists of two parts: a javascript client application and
a warp server.

###Client
To build the client run `build.sh` in `example/client` directory. The
only requirements
are [typescript compiler](https://github.com/Microsoft/TypeScript)
and [rollup](https://github.com/rollup/rollup).

Alternatively, the client can be built using
[nix](https://github.com/NixOS/nix) with command `nix-build
example.nix`.

Both methods create directory `example/client/result/static` required
to run the server.

###Server
Type `cabal run example` to launch a local server at 8080 port.
It follows the same idea as example server from
[servant-auth-cookie](https://github.com/zohl/servant-auth-cookie)
library.
