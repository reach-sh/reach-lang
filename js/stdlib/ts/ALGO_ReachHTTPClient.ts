import type { BaseHTTPClient, BaseHTTPClientError, BaseHTTPClientResponse } from "algosdk"
import type { Query } from "algosdk/dist/types/src/client/baseHTTPClient";

export type Method = 'get' | 'post' | 'delete';
export type EventName = 'before' | 'success' | 'error';
export type EventDetails = {
  method: Method,
  relativePath: string,
  data?: Uint8Array,
  query?: Query<string>,
  requestHeaders?: Record<string,string>,
  response?: BaseHTTPClientResponse,
  err?: BaseHTTPClientError | any,
}
export type Event = {
  eventName: EventName,
  reqNum: number,
  label: string,
} & EventDetails
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

  async _doTheThing(eventDetails: EventDetails, go: () => Promise<BaseHTTPClientResponse>): Promise<BaseHTTPClientResponse> {
    const { eh, reqNum, label } = this;
    this.reqNum = reqNum + 1;
    await eh({eventName: 'before', reqNum, label, ...eventDetails});
    try {
      const response = await go();
      await eh({eventName: 'success', reqNum, label, response, ...eventDetails});
      return response;
    } catch (err) {
      await eh({eventName: 'error', reqNum, label, err, ...eventDetails});
      throw err;
    }
  }

  async get(
    relativePath: string,
    query?: Query<string>,
    requestHeaders?: Record<string, string>
    ): Promise<BaseHTTPClientResponse> {
    const method = 'get';
    const eventDetails: EventDetails = { method, relativePath, query, requestHeaders};
    return await this._doTheThing(eventDetails, async () => await this.bc.get(relativePath, query, requestHeaders));
  }

  async post(
    relativePath: string,
    data: Uint8Array,
    query?: Query<string>,
    requestHeaders?: Record<string, string>
    ): Promise<BaseHTTPClientResponse> {
    const method = 'post';
    const eventDetails: EventDetails = { method, relativePath, data, query, requestHeaders }
    return await this._doTheThing(eventDetails, async () => await this.bc.post(relativePath, data, query, requestHeaders));
  }

  async delete(
    relativePath: string,
    data: Uint8Array,
    query?: Query<string>,
    requestHeaders?: Record<string, string>
    ): Promise<BaseHTTPClientResponse> {
    const method = 'delete';
    const eventDetails: EventDetails = { method, relativePath, data, query, requestHeaders }
    return await this._doTheThing(eventDetails, async () => await this.bc.delete(relativePath, data, query, requestHeaders));
  }
}
