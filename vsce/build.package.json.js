// so we can write a file at the very end of this
// program
const fs = require('fs');

const IMPORTED_COMMANDS = require("./data/commands.json");

const NEW_PACKAGE_JSON = require("./base.package.json");

const COMMANDS_ARRAY = [];
const MENUS_EXPLORERCONTEXT_ARRAY = [];
const MENUS_EDITORTITLE_ARRAY = [];
const MENUS_TOUCHBAR_ARRAY = [];
const MENUS_EDITORCONTEXT_ARRAY = [];

// Build the activationEvents array from scratch
// each time to avoid duplicate strings.
const ACTIVATION_EVENTS_ARRAY = [
	"workspaceContains:**/*.rsh",
	"workspaceContains:reach"
];

// **Every** command seems to be present in
// MAIN_JSON.contributes.menus["explorer/context"].
// **Every** command also seems to be present in
// MAIN_JSON.contributes.commands. See
// original.package.json.
IMPORTED_COMMANDS.forEach(commandObject => {
	// Extract a whole bunch of properties,
	// even if they don't exist, from the parameter.
	const {
		command, category, title, icon, when, group,
		touchBar, activationEvents
	} = commandObject;
	
	// The three categories command, category and
	// title should always exist. See
	// original.package.json.
	const newCommandObject = {
		command, category, title
	};

	// If this object has an icon property, add it!
	if (icon) {
		newCommandObject.icon = icon;
	}

	COMMANDS_ARRAY.push(newCommandObject);

	// More than one section of package.json just
	// needs an object with the form { command }.
	const recurringObjectShape = { command };
	
	// next part
	// The menus["explorer/context"] array just needs
	// objects that contain the command key/value
	// pair. Every command should have that command
	// key/value pair. See original.package.json.
	MENUS_EXPLORERCONTEXT_ARRAY.push(
		recurringObjectShape
	);

	// next part
	// Does this command have a "when" property?
	// If it does, add it to the menu["editor/title"]
	// array. See original.package.json.
	if (when) {
		const menuEditortitleObject = {
			when, command, group
		};
		MENUS_EDITORTITLE_ARRAY.push(
			menuEditortitleObject
		);
	}

	// next part
	// Does this command have a touchBar property?
	// If it does, add it to the appropriate array.
	if (touchBar) {
		MENUS_TOUCHBAR_ARRAY.push(
			recurringObjectShape
		);
	}

	// next part
	if (commandObject["explorer/context"]) {
		MENUS_EXPLORERCONTEXT_ARRAY.push(
			recurringObjectShape
		);
	}

	// next part
	if (commandObject["editor/context"]) {
		MENUS_EDITORCONTEXT_ARRAY.push(
			recurringObjectShape
		);
	}

	// next part
	if (activationEvents) {
		// Push just the string and not an entire
		// object, here. "activationEvents" is just
		// an array of strings.
		ACTIVATION_EVENTS_ARRAY.push(
			activationEvents
		);
	}
});

// Add all of the small pieces into the big piece!
// See original.package.json to see how a built
// package.json file is *supposed* to look.
NEW_PACKAGE_JSON.contributes.commands
	= COMMANDS_ARRAY;

NEW_PACKAGE_JSON.contributes.menus["explorer/context"]
	= MENUS_EXPLORERCONTEXT_ARRAY;

NEW_PACKAGE_JSON.contributes.menus["editor/title"]
	= MENUS_EDITORTITLE_ARRAY;

NEW_PACKAGE_JSON.contributes.menus.touchBar
	= MENUS_TOUCHBAR_ARRAY;

NEW_PACKAGE_JSON.contributes.menus["editor/context"]
	= MENUS_EDITORCONTEXT_ARRAY;

NEW_PACKAGE_JSON.activationEvents
	= ACTIVATION_EVENTS_ARRAY;

// Add a newline character for more identical diffs
fs.writeFileSync(
	'./package.json',
	JSON.stringify(NEW_PACKAGE_JSON, null, 4) + "\n"
);