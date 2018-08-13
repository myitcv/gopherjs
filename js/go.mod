module github.com/gopherjs/gopherjs/js

// We want to require github.com/gopherjs/gopherjs/v11 but VCS https://github.com/gopherjs/gopherjs does not
// know anything about v11, only https://github.com/myitcv/gopherjs does. Is this another instance of
// https://github.com/golang/go/issues/26241? Because any attempt to resolve v11 within https://github.com/gopherjs/gopherjs
// is going to fail... We also need some sort of "fake" version number here for that reason too, because
// whatever commit shows below will always result in the error:
//
// go: github.com/gopherjs/gopherjs/v11@v11.0.0-20180628210949-0892b62f0d9f: missing github.com/gopherjs/gopherjs/go.mod and .../v11/go.mod at revision 0892b62f0d9f
//
require github.com/gopherjs/gopherjs/v11 v11.0.0-20180628210949-0892b62f0d9f

replace (
	// branch introduce_v11
	github.com/gopherjs/gopherjs => github.com/myitcv/gopherjs v0.0.0-20180813162217-54a508725be7

	// branch introduce_v11
	github.com/gopherjs/gopherjs/js => github.com/myitcv/gopherjs/js v0.0.0-20180813162217-54a508725be7
)
