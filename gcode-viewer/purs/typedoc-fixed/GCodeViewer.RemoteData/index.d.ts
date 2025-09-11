import type * as __imp1 from "../GCodeViewer.RemoteData";

export const mkRemoteDataStatus : { readonly 'error': (_: { readonly 'message': string; }) => __imp1.RemoteDataStatus; readonly 'loaded': __imp1.RemoteDataStatus; readonly 'loading': __imp1.RemoteDataStatus; readonly 'notAsked': __imp1.RemoteDataStatus; }

export const onRemoteDataStatus : <Z>(_: { readonly 'error': (_: { readonly 'message': string; }) => Z; readonly 'loaded': Z; readonly 'loading': Z; readonly 'notAsked': Z; }) => (_: __imp1.RemoteDataStatus) => Z

export type RemoteDataStatus = { readonly '__brand': unique symbol; }