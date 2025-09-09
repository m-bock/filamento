import { useState } from "react";

type TsStateHandle<state> = {
  updateState: (state: (st: state) => () => state) => () => void;
  readState: () => state;
};

type TsApi<pubState, state, disp> = {
  readonly dispatchers: (state: TsStateHandle<state>) => disp;
  readonly initState: state;
  readonly getPubState: (state: state) => pubState;
};

export const useStateMachine = <pubState, state, disp>(
  tsApi: TsApi<pubState, state, disp>
): [pubState, disp] => {
  const [st, setSt] = useState<state>(tsApi.initState);

  const tsStateHandle: TsStateHandle<state> = {
    updateState: (stateFn) => () => setSt((st) => stateFn(st)()),
    readState: () => st,
  };

  const dispatchers: disp = tsApi.dispatchers(tsStateHandle);

  const pubState: pubState = tsApi.getPubState(st);

  return [pubState, dispatchers];
};
