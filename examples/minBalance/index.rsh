'reach 0.1';

import { useConstructor } from '@nash-protocol/starter-kit:util.rsh'
import { 
  Participants as AppParticipants,
  Views as AppViews, 
  Api as AppApi, 
  App 
} from '@nash-protocol/voting:interface.rsh'
export const main = Reach.App(() => 
  App(useConstructor(AppParticipants, AppViews, AppApi)));