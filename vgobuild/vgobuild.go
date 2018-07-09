package vgobuild

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"go/build"
)

var (
	goCmdName = "go"
	wd        string
	pkgMap    map[string]*build.Package
	useVgo    bool
)

func init() {
	var err error

	wd, err = os.Getwd()
	if err != nil {
		panic(err)
	}

	// Per https://go-review.googlesource.com/c/vgo/+/122260 we use the value of
	// env var GO111MODULE to determine what mode we are in.
	switch os.Getenv("GO111MODULE") {
	case "no":
	case "yes":
		useVgo = true
	default:
		// "auto" or unknown value
		// for now we simple walk from the current directory up to root

		cd := wd

		for {
			fn := filepath.Join(cd, "go.mod")
			fi, err := os.Stat(fn)
			if err == nil && !fi.IsDir() {
				useVgo = true
				break
			}

			d := filepath.Dir(cd)
			if d == cd {
				break
			}
			cd = d
		}
	}

	if useVgo {
		goCmdName = "vgo"
	}
}

func Import(c *build.Context, path, srcDir string, mode build.ImportMode) (*build.Package, error) {
	bp, err := c.Import(path, srcDir, mode)
	if err == nil && bp.Goroot {
		return bp, nil
	}

	pkg, ok := pkgMap[path]
	if !ok {
		return nil, fmt.Errorf("failed to resolve %v; not in pkgMap", path)
	}
	return pkg, nil
}

func ImportDir(c *build.Context, dir string, mode build.ImportMode) (*build.Package, error) {
	panic("ImportDir not implemented yet")
}

// Every GopherJS command resolves import path patterns via this method
// which means we can use it as a hook for pre-fetching all the vgo deps
// if we are in vgo mode
func ImportPaths(c *build.Context, patterns []string) ([]string, error) {
	if len(patterns) == 0 {
		patterns = []string{"."}
	}
	listCmd := exec.Command(goCmdName, append([]string{"list"}, patterns...)...)
	listCmd.Dir = wd
	listOut, err := listCmd.Output()
	if err != nil {
		// things will fail later per existing GopherJS behaviour
		return nil, fmt.Errorf("%v", err)
	}

	res := strings.Split(strings.TrimSpace(string(listOut)), "\n")

	if !useVgo {
		return res, nil
	}

	pkgMap = make(map[string]*build.Package)

	args := append([]string{"list", "-json", "-deps", "-test", "-tags", strings.Join(append(c.BuildTags, "js"), " ")}, res...)

	cmd := exec.Command(goCmdName, args...)
	cmd.Dir = wd

	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to run %q for Import in %v for context %#v: %v", strings.Join(cmd.Args, " "), cmd.Dir, c, err)
	}
	dec := json.NewDecoder(bytes.NewReader(out))
	for {
		var p build.Package
		err := dec.Decode(&p)
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, fmt.Errorf("failed to json decode for Import: %v", err)
		}
		pkgMap[p.ImportPath] = &p
	}

	return res, nil
}
