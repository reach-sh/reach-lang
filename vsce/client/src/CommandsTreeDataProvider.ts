import * as vscode from 'vscode';
import { TreeItem } from 'vscode';
import * as path from 'path';

const reach_icon = path.join(__filename, '..', '..', '..', 'images', 'reach-icon.svg');
const reach_icon_red = path.join(__filename, '..', '..', '..', 'images', 'reach-icon-red.svg');
const discord_icon = path.join(__filename, '..', '..', '..', 'images', 'discord-icon-small.png');
const github_icon = path.join(__filename, '..', '..', '..', 'images', 'github-icon-red.png');
const gist_icon = path.join(__filename, '..', '..', '..', 'images', 'github-icon-blue.png');

const makeTreeItem = (label, command, icon = reach_icon) => {
	return makeLabeledTreeItem(label, label, command, icon);
};

const makeLabeledTreeItem = (label: string, title: string, command: string, icon = reach_icon) => {
	const t : TreeItem = new TreeItem(title, 0);
	t.command = {
		command: command,
		title: label,
		arguments: []
	};
	t.iconPath = {
		light: vscode.Uri.parse(icon) ,
		dark: vscode.Uri.parse(icon)
	};
	return t;
};

export class CommandsTreeDataProvider implements vscode.TreeDataProvider<TreeItem> {

	data: vscode.TreeItem[];

	constructor() {
		this.data = [
			makeTreeItem('clean', 'reach.clean'),
			makeTreeItem('compile', 'reach.compile'),
			makeTreeItem('devnet', 'reach.devnet'),
			makeTreeItem('docker-reset', 'reach.docker-reset'),
			makeTreeItem('down', 'reach.down'),
			makeTreeItem('hashes', 'reach.hashes'),
			makeTreeItem('init', 'reach.init'),
			makeTreeItem('react', 'reach.react'),
			makeTreeItem('rpc-run', 'reach.rpc-run'),
			makeTreeItem('rpc-server', 'reach.rpc-server'),
			makeTreeItem('run', 'reach.run'),
			makeTreeItem('scaffold', 'reach.scaffold'),
			makeTreeItem('update', 'reach.update'),
			makeTreeItem('upgrade', 'reach.upgrade'),
			makeTreeItem('version', 'reach.version'),
		];
	}

	getTreeItem(element: TreeItem) {
		return element;
	}

	getChildren(_?: TreeItem|undefined) {
		return this.data;
	}
}

export class HelpTreeDataProvider implements vscode.TreeDataProvider<TreeItem> {

	data: vscode.TreeItem[];

	constructor() {
		this.data = [
			makeLabeledTreeItem('discord', 'Chat on Discord', 'reach.discord', discord_icon),
			makeLabeledTreeItem('gist', 'Create a Gist to share', 'reach.gist', gist_icon),
			makeLabeledTreeItem('issue', 'Open an issue on GitHub', 'reach.issue', github_icon)
		];
	}

	getTreeItem(element: TreeItem) {
		return element;
	}

	getChildren(_?: TreeItem|undefined) {
		return this.data;
	}
}

export class DocumentationTreeDataProvider implements vscode.TreeDataProvider<TreeItem> {

	data: vscode.TreeItem[];

	constructor() {
		this.data = [
			makeLabeledTreeItem('docs', 'Open the documentation', 'reach.docs', reach_icon_red),
		];
	}

	getTreeItem(element: TreeItem) {
		return element;
	}

	getChildren(_?: TreeItem|undefined) {
		return this.data;
	}
}