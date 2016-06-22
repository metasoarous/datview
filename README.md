# Datview

[Insert explosive graphic]



## Introduction

If om-next gets the idea of components requesting the shape of data they need correct, Datview goes one step further in realizing you can let the shape of the data you request direct the rendering of that data.

Instead of decorating components with information about where they get their data, decorate _queries_ with information about how they render :-)
This data driven approach leads to effortlessly composable view code, meaning complex UI can be programmatically constructed from simple pieces.
This will make it possible to very rapidly build complex data driven applications.

It's going to take a bit of work to fully smooth out all the defaults and patterns, but I'm liking the picture so far.


## How does it work?

We do this with metadata on parts of our pull expressions and queries

Example:


```clj
    (def small-fonts {:font-size "8px"})
    (def small-fonts-bold (merge small-fonts
                                 {:font-weight "bold"}))

    (def time-entry-view
      ^{:attributes {:e/description {:style small-fonts}}
        ;; Possible? But not supported yet.
        :derived-attributes {:time.entry/duration
                             ^{:datview.derived-attribute/of [:time.entry/stop-time :time.entry/start-time]}
                             (fn [{:as time-entry :keys [time.entry/stop-time time.entry/start-time]}]
                               (- stop-time start-time))}}
      [:e/description :time.entry/duration]

    (def todo-view
      ^{:attributes {:e/tags {:style small-fonts-bold :summarize tag-name-fn}
                     :e/description {:style small-fonts}
                     :e/category {:style small-fonts-bold}
                     :todo/hours {:wrapper todo-hours-with-summary}}
        :wrapper [lined-box]}
      [:e/name :e/category :e/tags :e/description
       ;; Here we have some reference attributes
       {:todo/time-entries time-entry-view}
       {:todo/subtasks ^{:note "Here merge into the attributes passed down recursively"}
                       '...}])
```


Functions (Reagent components) like `pull-view`, `attr-view` and `value-view` are wired together into a recursive tree, based on various entry points.
Each one of these entry points can be customized in the structure of the pull metadata.
Thus everything is perfectly composable, because everything is **just data**.
We can override things so that when we push down into some particular part of a pull expression, the corresponding components will be rendered exactly as you wish :-)

The brilliant thing is that we can also just do this if you don't need customization:

    (pull-view conn [:e/name :e/category :e/tags :e/description {:todo/subtasks ...}])
    ;; uhh... actually not _quite_ yet... need to get `'...` to work in posh

Or even better

    (pull-view conn '[*] eid)

Fine...

    (entity-view conn eid)

Collections?

Yeah, we got that too:

    (pull-many-view conn todo-view todo-eids)
    ;; Just kidding! Coming soon...

What about q?

    (q-view conn {:find [[('pull todo-view '?todo) '...]]
                  :where '[[?todo :e/type :e.type/Todo]
                           [?todo :e/category :category/Work]]})
    ;; Haha; Also j/k. Vaporware Suckers (TM)!

This lets us build tables or other collection views using the full expressiveness of DataScript Datalog for scope.

Imagine that?
Composing queries which know how to render themselves.

This schema is serializable to the DataScript DB, and can be accessed and operated upon in the component functions.
The default functions pull from these.
But you can customize and extend them more or less as you wish.
And of course, all of these settings are overridable by the metadata specifications on a local basis.


## Datview Specification Schema

Datview is half way towards using Prismatic Schema for type specifications.
However, now that Clojure 1.9 has proper specs, we'll probably redraft in terms of those.
Still, for the mean time, the specs in `datview.schema` should serve as a rough guide for the shape of the datview specification grammar.


## Customizing global defaults

You might have a set of global defaults (for styles, wrappers, rendering functions, etc) you'd like to apply without having to manually pass in the metadata each time.

We support this with the following functions:

* `default-config`: Returns a reaction of the default configuration.
* `update-default-config!`: Atomically update default config using an update function.
* `set-default-config!`: Resets the default-config.



