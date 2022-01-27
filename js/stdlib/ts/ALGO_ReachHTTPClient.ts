import type { BaseHTTPClient, BaseHTTPClientError, BaseHTTPClientResponse } from "algosdk"
import type { Query } from "algosdk/dist/types/src/client/baseHTTPClient";

export type Method = 'get' | 'post' | 'delete';
export type EventDetails = {
  label: string,
  reqNum: number,
  method: Method,
  relativePath: string,
  data?: Uint8Array,
  query?: Query<string>,
  requestHeaders?: Record<string,string>,
}
export type Event = ({
  eventName: 'before',
} | {
  eventName: 'success',
  response: BaseHTTPClientResponse,
} | {
  eventName: 'error',
  err: BaseHTTPClientError | any,
}) & EventDetails
export type EventHandler = (event: Event) => Promise<unknown>

export class ReachHTTPClient implements BaseHTTPClient {
  bc: BaseHTTPClient;
  eh: EventHandler;
  label: string;
  reqNum: number;

  constructor(bc: BaseHTTPClient, label: string, eventHandler: EventHandler) {
    this.bc = bc;
    this.eh = eventHandler;
    this.label = label;
    this.reqNum = 0;
  }

  async _doTheThing(
    method: Method,
    relativePath: string,
    dataMay: [ Uint8Array | undefined ] | [],
    query: Query<string> | undefined,
    requestHeaders: Record<string,string> | undefined,
  ): Promise<BaseHTTPClientResponse> {
    const { eh, reqNum, label } = this;
    const eventDetails: EventDetails = {
      label, reqNum, method, relativePath, data: dataMay[0], query,
    };
    this.reqNum = reqNum + 1;
    await eh({eventName: 'before', ...eventDetails});
    try {
      const response = await this.bc[method](
        // @ts-ignore
        relativePath, ...dataMay, query, requestHeaders,
      );
      await eh({eventName: 'success', response, ...eventDetails});
      return response;
    } catch (err) {
      await eh({eventName: 'error', err, ...eventDetails});
      throw err;
    }
  }

  async get(
    relativePath: string,
    query?: Query<string>,
    requestHeaders?: Record<string, string>
    ): Promise<BaseHTTPClientResponse> {
    return await this._doTheThing('get', relativePath, [], query, requestHeaders);
  }

  async post(
    relativePath: string,
    data: Uint8Array,
    query?: Query<string>,
    requestHeaders?: Record<string, string>
    ): Promise<BaseHTTPClientResponse> {
    return await this._doTheThing('post', relativePath, [data], query, requestHeaders);
  }

  async delete(
    relativePath: string,
    data: Uint8Array,
    query?: Query<string>,
    requestHeaders?: Record<string, string>
    ): Promise<BaseHTTPClientResponse> {
      return await this._doTheThing('delete', relativePath, [data], query, requestHeaders);
  }
}
