-record(literal, {value = undefined :: istype:form()}).

-record(type, {module = erlang    :: module() | undefined,
               type   = undefined :: atom(),
               spec   = []        :: istype:type_spec(),
               params = []        :: istype:forms()}).

-record(record, {arity    = 0         :: integer(),
	             record   = undefined :: atom(),
	             fields   = []        :: list(atom()),
	             types    = #{}       :: #{atom() := atom()},
	             defaults = #{}       :: #{atom() := atom()}}).