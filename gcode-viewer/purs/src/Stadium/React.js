import { useState } from "react";

export const useStateMachine = (tsApi) => () => {
  const [ref, setSt] = useState({ state: tsApi.initState });

  const tsStateHandle = {
    updateState: (stateFn) => () =>
      setSt((ref) => {
        ref.state = stateFn(ref.state)();
        return ref;
      }),
    readState: () => {
      return ref.state;
    },
  };

  const dispatch = tsApi.dispatchers(tsStateHandle);
  const state = ref.state.pubState;

  return { state, dispatch };
};
