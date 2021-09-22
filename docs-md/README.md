You need to install the vuepress-next fork:

https://github.com/vuepress/vuepress-next/pull/388

And you need to build it (with yarn) and then link it

```
lerna exec -- yarn link
```

Then link all of those things to this package

```
for i in ~/.config/yarn/link/@vuepress/* ; do echo ${i} ; done | sed "sJ/${HOME}
.config/yarn/link/JJ" | xargs yarn link
yarn link vuepress
yarn link vuepress-vite
```

Then you need to make sure that the fork's shiki is the same as ours:

```
ln -s $(pwd)/forks/vuepress-next/node_modules/shiki node_module
```
