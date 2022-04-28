export interface ActorSet {
    [key: number]: string
}

export interface Actor {
        l_acct: number
        l_ivd: any
        l_ks: string
        l_livs: any
        l_phase: number
        l_store: Array<store>
        l_who: string
    }

export type Participant = {
  actorId: number;
  nodeId: number;
  actorSet: any;
  apiSet: any;
  who: string;
  phase: string;
};
    
export type Locals = {
        l_curr_actor_id: number
        l_locals: Array<Actor>
    }

export type SimulatorObject = {

}

export type apiB ={
    a_name: string
}

export type actorSet = {

}

export type apiSet = {
    a_liv: any
    a_name: string

}

export type nodeId = {

}

export type storePayload = {
    tag: string
    contents: any
}

export type store = {
    0 : string
    1 : storePayload
}

export type Node = {
    
}