import { useContext } from "react";

import { ReachContext } from "../context/ReachContext";

export const useReach = () => {
	return useContext(ReachContext);
};
