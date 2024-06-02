import React from "react";
import ReactDOM from "react-dom";
import { useReach } from "../hooks/useReach";

export function renderDOM(app) {
	ReactDOM.render(
		<React.StrictMode>{app}</React.StrictMode>,
		document.getElementById("root")
	);
}

const RenderViews = (Views) => {
	const { views } = useReach();
	const View = Views[views.view];
	const Wrapper = views.Wrapper ? Views[views.Wrapper] : Views["AppWrapper"];

	const content = <View />;
	return <Wrapper>{content}</Wrapper>;
};

export default RenderViews;
