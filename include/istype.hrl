-record(literal, {value = undefined :: istype:form()}).

-record(type, {module = undefined :: module() | undefined,
               type   = undefined :: atom(),
               spec   = []        :: istype:type_spec(),
               params = []        :: istype:forms()}).