# Haskell: jump-to-definition in Emacs

## Motivation

Jump-to-definition doesn't work well for Haskell in Emacs. It didn't work well with Intero, it doesn't work well with Dante, it isn't going to work well with `haskell-ide-engine` in the foreseeable future, [it seems](https://github.com/haskell/haskell-ide-engine/issues/308).

The lack of a decent jump-to-definition has been bugging me for ages. Here are some of the problems:

1. you're in the `test` folder of your project and you want to jump into a module from `src`. Dante fails to do that, you can only jump within the same Cabal stanza (i.e. within a single `library`, `executable`, `test-suite`, `benchmark` etc appearing in your `*.cabal` file). As you can imagine (and probably already know), this is extremely annoying
2. your code doesn't type check? Bye-bye jump-to-definition
3. you're working on two different Cabal projects (whether they share the same `stack.yaml` file does not affect anything) and one of them depends on the latter. Occasionally you want to jump from the downstream project into the upstream one. Guess what, you can't
4. wanna navigate the source code of a dependency like `base` or `lens`? You want too much

This post describes how to mitigate these problems.

The post is about Emacs, but some of it applies to Vim as well.

## Disclaimer

My Emacs skills are nearly non-existent, so there may exist better ways of doing what I'm doing here. I'd appreciate any ideas on possible improvements of the presented solution.

## Jumping from one Cabal stanza into another one within a single Cabal project

We're going to use [Emacs Tags](https://www.emacswiki.org/emacs/EmacsTags). For populating the `TAGS` file I use [hasktags-emacs](https://github.com/ptek/hasktags-emacs) (which is build upon [hasktags](https://hackage.haskell.org/package/hasktags)), open the link and follow the instructions to set `hasktags-emacs` up (note that if you're a Stack user you can use `stack install` instead of `cabal install`).

So what `hasktags-emacs` does is that it repopulates the `TAGS` file of a project on each save. Normally, the `TAGS` file is not big -- on the order of several dozens of kilobytes, so regenerating tags from scratch on each save does not seem to be too annoying, but for big projects it might be a pain and we'll consider how to handle those below.

The `generate-hasktags.sh` script of `hasktags-emacs` generates tags in both the Emacs and Vim formats via

```
generateTags() {
  hasktags -b .
}
```

I don't need Vim tags, so in order not to generate them I changed the above code to

```
generateTags() {
  hasktags -e .
}
```

I replaced the

```elisp
(add-hook 'after-save-hook 'hasktags)
```

line in `hasktags.el` of `hasktags-emacs` with this one:

```elisp
(add-hook 'haskell-mode-hook
  (lambda () (add-hook 'after-save-hook 'hasktags nil 'local)))
```

to ensure that `generate-hasktags.sh` only runs when editing a Haskell file.

On save `hasktags-emacs` will recursively search up the directory tree to find the `TAGS` file and repopulate it if it exists. If there is no such file, nothing happens. So you need to create such a file manually. The root of your project is a good place to create it in, unless the project is huge.

You'll also need to tell Emacs how to locate the `TAGS` file using the same "recursively search up the directory tree" logic. I've found this piece of code on the Internet (**full code from now on is in the `code.el` file**):

```elisp
(defun jds-find-tags-file ()
  <...>)

(defun jds-set-tags-file-path ()
  "calls `jds-find-tags-file' to recursively search up the directory tree to find
a file named 'TAGS'. If found, adds it to 'tags-table-list', otherwise raises an error."
  (interactive)
  (add-to-list 'tags-table-list (jds-find-tags-file)))

(add-hook 'haskell-mode-hook 'jds-set-tags-file-path)
```

Once a `*.hs` file is opened, the hook fires and `jds-set-tags-file-path` gets called. It locates the `TAGS` file and adds it to the list of available tags tables (the last line of the `jds-set-tags-file-path`). If the located `TAGS` file is already there, nothing happens (that's the logic of `add-to-list`).

I use Dante, so `M-.` is already bound for me. Dante's jump-to-definition works much better than the via-tags one, because it knows what the thing under the cursor exactly is (hence the requirement to type check your code beforehand), so there's never any ambiguity, but as discussed above Dante's jump-to-definition fails in way too many cases. So it's a sensible default and I keep it under `M-.` and add `C-M-.` for the via-tags jump-to-definition:

```elisp
(defun my-xref-find-etags ()
  (interactive)
  (let* ((xref-backend-functions '(etags--xref-backend))
         (thing (xref-backend-identifier-at-point 'etags)))
    (xref-find-definitions thing)))

(global-set-key (kbd "C-M-.") 'my-xref-find-etags)

```

The via-tags jump-to-definition is pretty stupid: it literally just searches for the string name of the identifier under the cursor, so if you reuse the same name for different things in different modules, it's not going to work well for you. And sometimes it fails even in unambiguous cases (see for example [this irritating bug](https://github.com/MarcWeber/hasktags/issues/76)). Still, most of the time it does the job.

Haskell is case-sensitive, hence you probably want to reflect that, so that jumping to the definition of `const` doesn't result in a tedious process of explicitly choosing between `const` and `Const` via a pop-up menu or the like. That can be achieved via

```elisp
(custom-set-variables
  <...>
 '(tags-case-fold-search nil))
```

## Jumping into a different Cabal project

Say you have a Stack project with a bunch of different Cabal projects inside. You can create a single `TAGS` file in the root of the Stack project and it'll work for all the Cabal projects. But if the Stack project is a big one, then you probably don't want to overwrite the `TAGS` file (that has tags for all the Cabal projects) on each save. Especially if you don't normally work in all of the Cabal projects at the same time. What you can do here is put a `TAGS` file into the root directory of each of the Cabal projects.

Then for each Emacs session you'll need to open a `*.hs` file from each of the Cabal projects in order to load all of the `TAGS` files and you're good to go.

You can also add

```elisp
(custom-set-variables
  <...>
 '(tags-table-list (quote ("<path-to-project1>/TAGS" "<path-to-project2>/TAGS" "<path-to-project3>/TAGS"))))
```

to your `~/.emacs` file and the `TAGS` files of `project1`, `project2` and `project3` will be loaded automatically on start.

Or you can set the value of `tags-table-list` via `.dir-locals.el`, which is a file that contains directory local variables. Content of `.dir-locals.el` should look something like this:

```elisp
((haskell-mode . (tags-table-list . ("<path-to-project1>/TAGS" "<path-to-project2>/TAGS"))))
```

Then every time you enter the directory the `TAGS` files of `project1` and `project2` will be loaded. Didn't try that, though.

## Jumping into third-party libraries

You can generate tags for a project __and its dependencies__ via [haskdogs](https://hackage.haskell.org/package/haskdogs). The dependencies have to be explicitly imported in `.hs` files of the project (`haskdogs` scans import sections of `*.hs` files). I have a folder called `registry` where I store a Haskell project with a single `*.hs` file that only imports stuff from `base`, `containers`, `mtl`, `lens`, etc. Running

```
haskdogs --hasktags-args "-e"
```

over it generates a ~1 MB TAGS file (I thought it was bigger), which I then load by default via

```elisp
(custom-set-variables
  <...>
 '(tags-table-list (quote ("<path-to-the-registry-folder>/TAGS"))))
```

Now whenever I want to jump into `base` or `lens` from any Haskell project, I hit `C-M-.` and it works.

## Conclusions

Setting up all of this is tiresome, but now that I've been using this machinery for a few months, I can't think of not having jump-to-definition working in lots of important cases.

If you liked the post and appreciate the effort, consider [sponsoring](https://github.com/sponsors/effectfully-ou) this blog (starts from 1$).
