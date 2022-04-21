import { execSync, spawn } from 'child_process';
import {
  OutputChannel,
  ProgressLocation,
  ProgressOptions,
  window
} from 'vscode';

import VERSION_COMPARE_JSON_USING
  from './spawnVersionCompareJson';

const ASYNC_UPDATE_USING = (
  pathToScript: string,
  versionCompareJson: VersionCompareJson,
  outputChannel: OutputChannel
): Promise<boolean> => {
  const {
    dockerC, dockerH, noDiff, script
  } = versionCompareJson;
  outputChannel.appendLine('dockerC ' + dockerC);
  outputChannel.appendLine('dockerH ' + dockerH);
  outputChannel.appendLine('noDiff ' + noDiff);
  outputChannel.appendLine('script ' + script);

  const argsArray: string[] = [
    'update-ide',
    `--json=${dockerC}`,
    '--rm-json'
  ];

  if (noDiff || script) argsArray.push('--script');

  return new Promise(resolve => {
    const process = spawn(
      pathToScript, argsArray
    );

    process.on('error', error => {
      console.info('update-ide\'s process', process);
      console.info('update-ide\'s error', error);

      outputChannel.appendLine('error message:');
      outputChannel.appendLine(error.message)
      outputChannel.appendLine('error name:');
      outputChannel.appendLine(error.name)
      outputChannel.appendLine('error stack:');
      outputChannel.appendLine(error.stack);
      outputChannel.appendLine('stdout:');
      outputChannel.appendLine(
        process.stdout.read(
          process.stdout.readableLength
        )
      );
      outputChannel.appendLine('stderr:');
      outputChannel.appendLine(
        process.stderr.read(
          process.stderr.readableLength
        )
      );

      resolve(false);
    });

    process.on('exit', (code, signal) => {
      console.info('update-ide\'s process', process);
      outputChannel.appendLine(
        `\nupdate-ide exited with code ${
          code
        } and signal ${signal}. ` +
        new Date().toLocaleTimeString()
      );

      if (code === 0) {
        return resolve(true);
      }

      outputChannel.appendLine(
        'Updating was unsuccessful. ' +
        new Date().toLocaleTimeString()
      );
      outputChannel.appendLine('stdout:');
      outputChannel.appendLine(
        process.stdout.read(
          process.stdout.readableLength
        )
      );
      outputChannel.appendLine('stderr:');
      outputChannel.appendLine(
        process.stderr.read(
          process.stderr.readableLength
        )
      );

      resolve(false);
    });
  });
};

/**
 * @param pathToScript the path to download the script to
 * @returns an error if something went wrong
 */
const UPDATE_SCRIPT_AND_CLI_IMAGE_USING = (
  pathToScript: string
) => {
  try {
    execSync('docker pull "reachsh/reach-cli:latest"');

    execSync(`curl https://docs.reach.sh/reach -o "${
      pathToScript
    }"`);

    execSync(`chmod +x "${pathToScript}"`);

  } catch (error) {

    return error;
  }

  return null;
};

export default async (
  pathToScript: string,
  jsonFromVersionCompareJson: VersionCompareJson,
  versionCompareJsonDoesntWork: boolean,
  outputChannel: OutputChannel
) => {
  const cancellable: boolean = false;

  const location: ProgressLocation =
    ProgressLocation.Notification;

  const title: string = "Updating Reach...";

  const progressOptions: ProgressOptions = {
    cancellable, location, title
  };

  // The actual update logic is here!
  window.withProgress(progressOptions, () =>
    new Promise<boolean>((resolve, reject) => setTimeout(
      async () => {
        if (versionCompareJsonDoesntWork) {
          outputChannel.appendLine(
            '\nAbout to update your reach ' +
            'script,'
          );
          outputChannel.appendLine(pathToScript);
          outputChannel.append(
            'and reach-cli Docker image... '
          );
          outputChannel.appendLine(
            new Date().toLocaleTimeString()
          );

          const error = UPDATE_SCRIPT_AND_CLI_IMAGE_USING(
            pathToScript
          );

          if (error) {
            return reject(error);
          }

          outputChannel.appendLine(
            'version-compare --json ' +
            'originally didn\'t work, ' +
            'but it should, now.'
          );
          outputChannel.appendLine(
            new Date().toLocaleTimeString()
          );

          jsonFromVersionCompareJson =
            await VERSION_COMPARE_JSON_USING(
              pathToScript,
              outputChannel
            );
        }

        if (jsonFromVersionCompareJson) {
          outputChannel.appendLine(
            '\nStarting update... ' +
            new Date().toLocaleTimeString()
          );

          const success = await ASYNC_UPDATE_USING(
            pathToScript,
            jsonFromVersionCompareJson,
            outputChannel
          );

          if (success === false) {
            return resolve(false);
          }

          outputChannel.appendLine(
            'Updating was successful. ' +
            new Date().toLocaleTimeString()
          );

          return resolve(true);
        }

        resolve(false);
      }, 0
    ))
  ).then(success => success &&
    window.showInformationMessage(
      'Reach is now up to date.', 'Close (Esc)'
    ),
    (reason) => {
      outputChannel.appendLine(
        '\n\n\n\n\n\nUpdating was unsuccessful due to\n'
      );
      outputChannel.appendLine(reason);
      window.showErrorMessage(
        'Updating was unsuccessful due to\n' + reason
      );
    }
  );
};
