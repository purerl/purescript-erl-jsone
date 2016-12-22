-module(erl_data_jsone@foreign).
-export([coerce_/1,fromObject_/1,jsonNull/0,isNull/1,toNull_/3,toBoolean_/3,toNumber_/3,toString_/3,toArray_/3,toObject_/3,foldJson/7]).

coerce_(X) -> X.

fromObject_(X) -> {X}.

jsonNull() -> null.

isNull(null) -> true;
isNull(_) -> false.

toNull_(Nothing,Just,null) -> Just(null);
toNull_(Nothing,_,_) -> Nothing.

toBoolean_(Nothing,Just,X) when is_boolean(X) -> Just(X);
toBoolean_(Nothing,Just,X) -> Nothing.

toNumber_(Nothing,Just,X) when is_float(X) -> Just(X);
toNumber_(Nothing,Just,X) -> Nothing.

% TODO is binary
toString_(Nothing,Just,X) when is_binary(X) -> Just(X);
toString_(Nothing,Just,X) -> Nothing.

toArray_(Nothing,Just,X) when is_list(X) -> Just(X);
toArray_(Nothing,Just,X) -> Nothing.

toObject_(_,Just,{X}) when is_list(X) -> Just(X);
toObject_(Nothing,_,_) -> Nothing.

foldJson(IsNull, IsBool, IsNum, IsStr, IsArr, IsObj, J) -> case J of
  null -> IsNull(null);
  _ when is_boolean(J) -> IsBool(J);
  _ when is_float(J) -> IsNum(J);
  _ when is_binary(J) -> IsStr(J);
  [_] -> IsArr(J);
  {A} -> IsObj(A)
end.
