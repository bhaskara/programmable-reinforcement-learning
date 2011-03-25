(defpackage stratagus-env
  (:documentation "Package stratagus-env (s-env).  Defines <stratagus-env> class to communicate with a running instance of the stratagus game over a socket.  Make specific stratagus domains by subclassing this.  Exported symbols:

Class
-----
<stratagus-env> - class

Env states
----------
make-strat-es
strat-es-globl
strat-es-units
unit-player-id
unit-type
unit-loc
unit-hp
unit-r-amt
unit-status
unit-status-args
global-state-gold
global-state-wood
global-state-map

Constants
---------
*noop*

Methods inherited from env
--------------------------
do-action - actions must be a hashtable from unit id to action (see hrl/strat/docs/protocol.txt for what individual action commands look like).  Action may also be *noop*, in which case no command will be sent to that unit.
reset
at-terminal-state
get-state
io-interface
effectors

Other methods
-------------
send


Make specific stratagus subdomains by subclassing <stratagus-env>.  Subclasses should provide methods for is-terminal-state and compute-reward.")
  (:nicknames s-env units)
  (:use common-lisp
	socket
	utils
	gameinfo
	set
	mp
	create-env)
  (:export make-stratagus-env
	   distance
	   get-game-state
	   <stratagus-env>
	   unit
	   world
	   global-state
	   reset
	   at-terminal-state
	   *noop*
	   compute-reward
	   send
	   io-interface
	   get-state
	   do-action
	   make-strat-es
	   strat-es-units
	   strat-es-global
	   get-unit-loc
	   global-state-gold
	   global-state-wood
	   global-state-map
	   unit-player-id
	   unit-type
	   unit-loc
	   unit-hp
	   unit-r-amt
	   unit-status
	   unit-status-args
	   generate-map
	   get-unit-state
	   units-belonging-to
	   units-belonging-to-of-type
	   all-units
	   ;; BEGIN EXPORT FROM STRAT-CONSTANTS
	   build-cmd
	   noop-cmd
	   *action-noop* *action-stop* *action-move* *action-build*
	   *action-attack* *action-repair* *action-harvest* *action-dropoff*
	   *unit-footman* *unit-grunt* *unit-footman* *unit-grunt* *unit-peasant*
	   *unit-peon* *unit-ballista* *unit-catapult* *unit-knight* *unit-ogre*
	   *unit-archer* *unit-axethrower* *unit-mage* *unit-death-knigt* *unit-human-oil-tanker*
	   *unit-orc-oil-tanker* *unit-elven-destroyer* *unit-troll-destroyer* *unit-human-transport*
	   *unit-orc-transport* *unit-battleship* *unit-ogre-juggernaught* *unit-gnomish-submarine*
	   *unit-giant-turtle* *unit-gnomish-flying-machine* *unit-gobblin-zeppelin* *unit-gryphon-rider*
	   *unit-dragon* *unit-dwarves* *unit-goblin-sappers* *unit-gryphon-aviary* *unit-dragon-roost*
	   *unit-farm* *unit-pig-farm* *unit-human-barracks* *unit-orc-barracks* *unit-townhall* *unit-town-hall*
	   *unit-mine* *unit-gold-mine* *unit-elven-lumber-mill* *unit-troll-lumber-mill* *unit-stables*
	   *unit-ogre-mound* *unit-mage-tower* *unit-template-of-the-damned* *unit-human-foundry*
	   *unit-human-refinery* *unit-orc-foundry* *unit-orc-refinery* *unit-gnomish-inventor*
	   *unit-goblin-alchemist* *unit-church* *unit-altar-of-storms* *unit-human-watch-tower*
	   *unit-orc-watch-tower* *unit-great-hall* *unit-keep* *unit-stronghold* *unit-castle*
	   *unit-fortress* *unit-orc-blacksmith* *unit-human-blacksmith* *unit-human-shipyard*
	   *unit-orc-shipyard* *unit-human-wall* *unit-orc-wall* *unit-oil-patch* *unit-human-oil-platform*
	   *unit-orc-platform* 
	   *status-undefined*
	   *status-stop*
	   *status-move*
	   *status-attack*
	   *status-build*
	   *status-under-construction*
	   *status-repair*
	   *status-harvest*
	   *gold-costs*
	   
	   N
	   S
	   E
	   W
	   
	   pid
	   
	   gameinfo
	   unit
	   *socket*
	   ))

(in-package cl-user)

