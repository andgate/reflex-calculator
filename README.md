## Building

Requires nix. Additionally, you need to edit `/etc/nix/nix.config` for macosx.
```
substituters = https://cache.nixos.org https://nixcache.reflex-frp.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
sandbox = false
```
(On linux, `sandbox = true`).


The calculator is built with
```
nix-build -o frontend-result -A ghcjs.frontend
```

The results are in `frontend-result`.
They can be minified with
```
ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=node --externs=all.js.externs > all.min.js
```