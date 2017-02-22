# Revision history for raven


	## 0.1.5 -- 17-2-22

	* ServerNode pulled out of Server.

	* Resource node has been set up, for the moment it only handles logging.
	Logs are written to ~/.raven/log.  Most log event created.

	* :kill now kills the client and sends a kill the command to the sever
	(not a long term solution, but one that will persist till the client
	is improved).  The client simply stops, but the server sends out kill
	messages to all nodes, closing them safely.


	## 0.1.4 -- 17-2-19

	* Entry updated.  Now requires a summary function (which takes a vector
	of entries and returns formatted text), a function to return the NA value,
	and a readEntry function that starts with a normal type.  This has not
	been implemented via read so the Entry can be read and shown as is.
	Entry also now exports a getEntries function for translating a vector
	and a countNAs function for the summary system.

	* Table has a function, tableSummary witch uses the summary function exported
	by entry.

	* BasicEntry and BasicUnboundEntry have been separated to avoid import errors.

	* Stat is a module containing simple statistics functions, all arithmetic
	(not sampling), with multiple of each as types required.


	## 0.1.3 -- 17-2-12

	* REPL now persists its state via an MVar, all work is done via a node
	and sent back to the connection (not just an echo sever anymore).

	* Connections are handled by a connection node to smooth communication.


	## 0.1.2 -- 17-2-10

	* Echo server setup using Cloud Haskell.  Client server interaction done
	via transportation layer, not via nodes.  No internal work yet.


	## 0.1.1 -- 17-2-8

	* Basic REPL set up using the hint library.  This is a naive implementation
	that forces the REPL to be set up again each time.

	* First pass at an Entry class built, with two example data types.

	* Table datatype for data sets build and tested


	## 0.1.0.0  -- 17-1-22

	* Init
