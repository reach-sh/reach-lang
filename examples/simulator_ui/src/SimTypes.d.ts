declare module '@reach-sh/simulator-client' {
            export function apiCall (): any
            export function catGraph(): any
            export function getAPIs(): any
            export function getActions(nodeId: number, participant: number): any
            export function getEdges(): any
            export function getLoc(): any
            export function getStateGlobals(stateID: number): any
            export function getStateLocals(stateID: number): any
            export function getStates(): any
            export function getStatus(): any
            export function init(): any
            export function initDetails(a: number): any
            export function initFor(s, a, liv?, acc?: {s: number, a: number, liv: any, acc: boolean}): any
            export function interp(): any
            export function interpCommand(): any
            export function load(): any
            export function newAccount(): any
            export function newToken(): any
            export function ping(): any
            export function resetServer(): any
            export function respondWithVal(): any
            export function transfer(): any
            export function waitForPort(): any
            export function resetServer(): any
            export function load(): any
}
    
declare module 'cytoscape-klay'
declare module 'lodasync'
