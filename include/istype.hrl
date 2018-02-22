-record(literal, {value = undefined :: istype:form()}).

-record(type, {module = erlang    :: module() | undefined,
               type   = undefined :: atom(),
               spec   = []        :: istype:type_spec(),
               params = []        :: istype:forms()}).