import fnmatch
import os

from invoke import task


@task
def help(ctx):
    ctx.run("stack build && stack exec -- gfmake --help")


@task
def sample1(ctx):
    ctx.run("stack build && stack exec -- gfmake sample1.gfm.txt")


@task
def style(ctx):
    source_files = [
        os.path.join(root, filename)
        for location in ("app", "src", "test")
        for root, _, filenames in os.walk(location)
        for filename in fnmatch.filter(filenames, "*.hs")
    ]

    ctx.run("stack exec -- hlint app src test && stylish-haskell {} -i".format(" ".join(source_files)))
