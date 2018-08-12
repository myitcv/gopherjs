module github.com/gopherjs/gopherjs/v11

require (
	github.com/fsnotify/fsnotify v1.4.7
	github.com/gopherjs/gopherjs/js v0.0.0
	github.com/inconshreveable/mousetrap v1.0.0 // indirect
	github.com/neelance/astrewrite v0.0.0-20160511093645-99348263ae86
	github.com/neelance/sourcemap v0.0.0-20151028013722-8c68805598ab
	github.com/shurcooL/go v0.0.0-20180423040247-9e1955d9fb6e
	github.com/shurcooL/httpfs v0.0.0-20171119174359-809beceb2371
	github.com/shurcooL/vfsgen v0.0.0-20180711163814-62bca832be04
	github.com/spf13/cobra v0.0.3
	github.com/spf13/pflag v1.0.1
	golang.org/x/crypto v0.0.0-20180807104621-f027049dab0a
	golang.org/x/sys v0.0.0-20180807162357-acbc56fc7007
	golang.org/x/tools v0.0.0-20180803180156-3c07937fe18c
)

replace (
	// branch js_use_gopherjs - required because the resolution of
	// github.com/myitcv/gopherjs/js will walk via github.com/myitcv/gopherjs
	// and we need to tell the resolver where to go for github.com/myitcv/gopherjs.
	// js_use_gopherjs is fine
	github.com/gopherjs/gopherjs => github.com/myitcv/gopherjs v0.0.0-20180813154419-5cca94e93813

	// branch js_use_gopherjs
	github.com/gopherjs/gopherjs/js => github.com/myitcv/gopherjs/js v0.0.0-20180813154419-5cca94e93813

	// branch latest
	github.com/shurcooL/vfsgen => github.com/myitcv/vfsgen v0.0.0-20180812213343-816be997bc25
)
