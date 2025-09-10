import { useState } from "react";

type TsStateHandle<state> = {
  updateState: (state: (st: state) => () => state) => () => void;
  readState: () => state;
};

type FullState<pubState, privState> = {
  pubState: pubState;
  privState: privState;
};

type TsApi<pubState, privState, disp> = {
  readonly dispatchers: (
    state: TsStateHandle<FullState<pubState, privState>>
  ) => disp;
  readonly initState: FullState<pubState, privState>;
};

export const useStateMachine = <pubState, privState, disp>(
  tsApi: TsApi<pubState, privState, disp>
): [pubState, disp] => {
  const [st, setSt] = useState<FullState<pubState, privState>>(tsApi.initState);

  const tsStateHandle: TsStateHandle<FullState<pubState, privState>> = {
    updateState: (stateFn) => () => setSt((st) => stateFn(st)()),
    readState: () => st,
  };

  const dispatchers: disp = tsApi.dispatchers(tsStateHandle);

  const pubState: pubState = st.pubState;

  return [pubState, dispatchers];
};
