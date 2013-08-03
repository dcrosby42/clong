# Clojure ECS: Pondering input / output spec for Systems

## Patterns in Clong so far

### Udate a single component of a single entity 

**update-component**

* Find: one component at a time by type, within a given entity
* Update: the component
* Impl: update-component
* Eg: ball-cieling-system
* Drawbacks: requires that you find and specify the entity before invoking.  Feels a little too direct; enables patchy code.  I used this in a few places to target very speciifc entities in a failed-to-solve-the-problem-neatly fasion.  Eg, ball-cieling-system looks for a specially tagged, individual component of which there can only ever be one by this implementation (:ball) and runs fairly imperitive code that ends with a call to update-component.)
* Like it? **not much**, want to avoid explicit use of this tool at the system level.  

### Update all components of a given type, one at a time

**update-components**

* Find: one component at a time by type
* Update: the component
* Impl: update-components 
* Eg: box-mover-system
* Like it? **yes**.  simple, clean, easy to write, easy to read
* Drawbacks: no way to consider the entity in search or update

### Update all components of a given type, providing peer components as inputs

**update-components2**

* Find: tuples of components by their types, per entity
* Update: first component of the tuple
* Impl: update-components2
* Eg: controller-system
* Like it? **yes**. Handles semi-complex tasks, easy to express input spec and update logic clearly
* Drawbacks: abused a little by adding component types to the tuples (and the entities themselves) in order to cludge-in a bit of entity filtering


## Existing Systems

**box-mover-system**

Updates all :box components using [box,dt]

GOOD: uses update-components

Search       | Input         | Update
------------ | ------------- | ------------
[ctype :box] | box, dt       | box


**ball-cieling-system**

COLLISION DETECTION

Updates :box of *specific* entity tagged as the :ball (by having superfluous component of type :ball, and being the only entity with such a component) using value pulled from *another specific* entity :field's component :box. 

	Get the ball entity id
	Get it's :box
	Get the :box of the :field entity
	Check for box collision
	Update the y component of the ball's box's velocity component if collided

BAD: exploits knowledge and structure of entity storage.

BAD: lookup, dig, lookup, dig, if, calc, apply change.  No expression encapsulates what's going on.

Search       | Input         | Update
------------ | ------------- | ------------
[[entity [ctype :ball]] :box], [[entity [ctype :field]] :box]| ball-box, field-box       | box

	
**ball-paddle-system**

Similar to **ball-cieling-system** in that it gets the :ball and the "tagged" :paddle entities' :box components, check for collision, update :x component of ball's velocity.

COLLISION DETECTION

Search       | Input         | Update
------------ | ------------- | ------------
[[entity [ctype :ball]] :box], [[entity [ctype :paddle]] :box]| ball-box, paddle-box       | box


**controller-system**

GOOD: Search, input and update target nicely expressed

Search       | Input         | Update
------------ | ------------- | ------------
[[ctype :controls] + [ctype :controller-mapping]] | controls, controller-mapping       | controls

**paddle-movement-system**

OK: Search and input closely aligned except the goofy component-as-tag :paddle, otherwise ehavily relies on update-components2

Search       | Input         | Update
------------ | ------------- | ------------
[[ctype :box] + [ctype :box] + [ctype :controls]] | box, controls       | box

**slow-effect-system**

Compound: first applies changes to :slow-effect components, then updates [:box+:slow-effect] combos.

GOOD: Clean implementations using update-components and update-components2

TODO: Is there a pattern of composition that could be extracted?

Search       | Input         | Update
------------ | ------------- | ------------
[ctype :slow-effect] | slow-effect | slow-effect
[[ctype :box] + [ctype :slow-effect]] | box, slow-effect | box

**paddle-weapon-system / add-lasers**

Search       | Input         | Update
------------ | ------------- | ------------
[[ctype :controls] + [ctype :box] + [ctype :paddle] + [ctype :id]] | controls, box, paddle-id | MANAGER: add laser entity

**paddle-weapon-system / collide-lasers-paddles**

COLLISION DETECTION

BAD: add-slow-effect iterates over paddle ids and uses em/set-component.  bad?


Search       | Input         | Update
------------ | ------------- | ------------
[entity [ctype :laser]], [entity [ctype :paddle]] | lasers' boxes, paddles' boxes | MANAGER: add explosion entities, remove collided lasers.  PADDLES: add slow effect

**paddle-weapon-system / expire-lasers**

LESS GOOD: remove-timed-out-entites, though reusable, reduces search-components results for :timer+ctype to a list of eids and invokes remove/entities.

Search       | Input         | Update
------------ | ------------- | ------------
[entity-id [ctype :timer + ctype]], [entity [ctype :paddle]] | lasers' boxes, paddles' boxes | MANAGER: add explosion entities, MANAGER remove collided lasers.  PADDLES: add slow effect

**paddle-bounds-system**

GOOD: implemented with update-components2 

BAD: uses hard-coded clamp values that coincide with the field instead of using the field's bounds… avoids the problem that **ball-cieling-system** solves awkwardly.

Search       | Input         | Update
------------ | ------------- | ------------
[ctype :box + :paddle] | box, [0,270 hardcoded vals] | box 

**goal-system**

BAD!: first-overlapping-goal exploits direct-entity structure to extract components

Search       | Input         | Update
------------ | ------------- | ------------
[[entity [ctype :ball]] :box], [[entity [ctype :goal]] :box], [[entity [ctype :id]] | goal entities, ball entity, scorer entity id | scorer's :score, MANAGER: change-to-mode :scored

**game-control-system**

QUESTIONABLE: uses get-mode on manager, case stmt that invkoes change-to-mode

Search       | Input         | Update
------------ | ------------- | ------------
[ctype :controls + :game-control] | MANAGER's :mode, controls | MANAGER: change-to-mode
 
**timer-system**

Search       | Input         | Update
------------ | ------------- | ------------
[ctype :timer] | timer | timer

**scored-system**

BAD: search for unique entity then exploit entity structure knowledge to get :timer component from :scored-timer entity

Search       | Input         | Update
------------ | ------------- | ------------
[component match :id = :scored-timer] | timer | MANAGER: change-to-mode

**explosion-system / expire-explosions**

LESS GOOD: remove-timed-out-entites, though reusable, reduces search-components results for :timer+ctype to a list of eids and invokes remove/entities.

Search       | Input         | Update
------------ | ------------- | ------------
[ctype :timer + :explosion] | seq of expired explosions | MANAGER: remove-entities

 
## Search and Update Scenarios

	[ :box ]  ->  { :box box, :manager mgr, :dt 0.013, :input input } -> 

box-mover-system

	[:box]  ->  (upfunc [box dt]  
	                 (m/update-mover box dt))

paddle-bounds-system

	Search    [:box [include? :tags :paddle]] 
	Updates   box 
	Using     (upfunc [[:box position :as box]]
	                                      (assoc box :position [x (clamp 0 270 y)]))

ball-cieling-system

	[:box [:tags include :ball]] -> (upfunc ])
	
## Could I use Graph?

Systems are a good candidate for fnk

Modes are composed sets of systems

* First I declare a bunch of named systems
* Then I explicitly chain the systems as the implementation of a particular mode

```
{:timer  (fnk [manager dt input] …)
 :scored (fnk [manager dt input] …)
 :mode-update (fnk [timer scored] (compose-chain timer scored))}
```
	