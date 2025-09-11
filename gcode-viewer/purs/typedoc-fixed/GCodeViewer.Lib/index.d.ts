import type * as __imp1 from "../GCodeViewer.Lib";

export type TsApi<msg, pubState, privState, disp> = { readonly 'dispatchers': (_: { readonly 'readState': () => __imp1.FullState<msg, pubState, privState>; readonly 'updateState': (_: (_: __imp1.FullState<msg, pubState, privState>) => () => __imp1.FullState<msg, pubState, privState>) => () => void; }) => disp; readonly 'initState': __imp1.FullState<msg, pubState, privState>; readonly 'timeTravel': (_: number) => void; }

export type FullState<msg, pubState, privState> = { readonly 'history': Array<{ readonly 'msg': msg; readonly 'pubState': pubState; }>; readonly 'historyIndex': number; readonly 'privState': privState; readonly 'pubState': pubState; }