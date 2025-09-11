import { useState } from "react";

type TsStateHandle<state> = {
  updateState: (state: (st: state) => () => state) => () => void;
  readState: () => state;
};

type FullState<msg, pubState, privState> = {
  pubState: pubState;
  privState: privState;
  history: { msg: msg; pubState: pubState }[];
  historyIndex: number;
};

type TsApi<msg, pubState, privState, disp> = {
  readonly dispatchers: (
    state: TsStateHandle<FullState<msg, pubState, privState>>
  ) => disp;
  readonly initState: FullState<msg, pubState, privState>;
};

export const useStateMachine = <msg, pubState, privState, disp>(
  tsApi: TsApi<msg, pubState, privState, disp>
): [pubState, disp] => {
  const [st, setSt] = useState<FullState<msg, pubState, privState>>(
    tsApi.initState
  );

  const tsStateHandle: TsStateHandle<FullState<msg, pubState, privState>> = {
    updateState: (stateFn) => () => setSt((st) => stateFn(st)()),
    readState: () => {
      return st;
    },
  };

  const dispatchers: disp = tsApi.dispatchers(tsStateHandle);

  const pubState: pubState = st.pubState;

  return [pubState, dispatchers];
};
