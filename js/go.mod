module github.com/gopherjs/gopherjs/js

require github.com/gopherjs/gopherjs v0.0.0-20180628210949-0892b62f0d9f

replace (
	// branch use_js_submodule
	github.com/gopherjs/gopherjs => github.com/myitcv/gopherjs v0.0.0-20180813154045-cf46d4a6e8b0

	// branch js_submodule
	github.com/gopherjs/gopherjs/js => github.com/myitcv/gopherjs/js v0.0.0-20180813130410-80a55d72fc42
)
