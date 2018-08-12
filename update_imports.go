// +build fiximports

package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"go/build"
	"go/format"
	"go/parser"
	"go/token"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

const (
	debug = false
)

var (
	fRewrites = flag.String("r", "-", "file containing rewrites, space-separated, one per line; defaults to stdin")
	fDebug    = flag.Bool("debug", false, "print debug output")
)

// TODO add flag that undoes the fact this tools ignores any build constraints; maybe
// this is a -tags flag which allows the user to specify the build containts (which could
// be empty)

func main() {
	flag.Parse()

	var rf *os.File
	if *fRewrites == "-" {
		rf = os.Stdin
	} else {
		f, err := os.Open(*fRewrites)
		if err != nil {
			fatalf("failed to open rewrites file: %v", err)
		}
		rf = f
	}

	rewrites := make(map[string]string)

	sc := bufio.NewScanner(rf)
	lineNo := 0
	for sc.Scan() {
		line := sc.Text()
		parts := strings.Split(strings.TrimSpace(line), " ")
		lineNo++
		if len(parts) != 2 {
			fatalf("rewrites had unexpected format on line %v: saw %q", lineNo, line)
		}
		rewrites[strings.TrimSpace(parts[0])] = strings.TrimSpace(parts[1])
	}
	if err := sc.Err(); err != nil {
		fatalf("could not parse rewrites: %v", err)
	}

	if len(rewrites) == 0 {
		infof("no rewrites to apply")
		os.Exit(0)
	}

	infof("Will apply rewrites:")
	for k, v := range rewrites {
		infof(" %q => %q", k, v)
	}

	patterns := flag.Args()
	if len(patterns) == 0 {
		patterns = []string{"."}
	}
	cmd := exec.Command("go", append([]string{"list"}, patterns...)...)
	out, err := cmd.Output()
	if err != nil {
		fatalf("failed to resolve package list from patterns: %v", err)
	}
	pkgs := strings.Split(strings.TrimSpace(string(out)), "\n")

	debugf("will work through packages %v", pkgs)

	fset := token.NewFileSet()

	for _, pkg := range pkgs {
		bpkg, err := build.Import(pkg, ".", 0)
		if err != nil {
			fatalf("failed to resolve pattern %v: %v", pkg, err)
		}
		files := []string{}
		files = append(files, bpkg.GoFiles...)
		files = append(files, bpkg.CgoFiles...)
		files = append(files, bpkg.IgnoredGoFiles...)
		files = append(files, bpkg.TestGoFiles...)
		files = append(files, bpkg.XTestGoFiles...)

		// the build.Import would fail if there were no go files...
		for _, fn := range files {
			fn = filepath.Join(bpkg.Dir, fn)
			f, err := parser.ParseFile(fset, fn, nil, parser.ParseComments)
			if err != nil {
				fatalf("failed to parse %v: %v", fn, err)
			}

			changed := false

			for _, i := range f.Imports {
				path := strings.Trim(i.Path.Value, "\"")
				if v, ok := rewrites[path]; ok {
					changed = true
					i.Path.Value = "\"" + v + "\""
				}
			}

			if !changed {
				infof("skipped: %v", fn)
				continue
			}

			bf := bytes.Buffer{}
			if err := format.Node(&bf, fset, f); err != nil {
				fatalf("failed to print rewrite of %v: %v", fn, err)
			}

			if err := ioutil.WriteFile(fn, bf.Bytes(), 0644); err != nil {
				fatalf("failed to write %v: %v\n", fn, err)
			}
			infof("rewrote: %v", fn)
		}
	}
}

func debugf(format string, args ...interface{}) {
	if debug || *fDebug {
		infof(format, args...)
	}
}

func infof(format string, args ...interface{}) {
	if format[len(format)-1] != '\n' {
		format += "\n"
	}
	fmt.Fprintf(os.Stderr, format, args...)
}

func fatalf(format string, args ...interface{}) {
	infof(format, args...)
	os.Exit(1)
}
