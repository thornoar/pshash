# `phash` -- the functional password manager

`phash` is a hashing algorithm implemented in Haskell, that serves as a password manager by accepting three keys (one public and two private) and returning a hash that can be used as a password. The program does not store the passwords anywhere, instead it generates them on the fly every time, which ensures some degree of security. The algorithm was designed to withstand brute-forcing as well. For finer detail, please refer to the corresponding mathematical paper: *documentation/main.pdf*

You can contact me via email: `r.a.maksimovich@gmail.com`, or on Telegram: `@thornoar`.
