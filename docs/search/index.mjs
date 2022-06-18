import algoliasearch from 'algoliasearch';
import fs from 'fs/promises';
import https from 'https';

const retriveAllGitHubDiscussions = () => {
  const options = {
    hostname: 'api.github.com',
    path: '/graphql',
    method: 'POST',
    headers: {
      'Authorization': `Bearer ${
        process.env['GITHUB_GRAPHQL_KEY']
      }`,
      'User-Agent': 'reach',
    }
  };

  return new Promise((resolve, reject) => {
    const request = https.request(options, response => {
      let data = '';

      response.on('data', chunk => {
        data += chunk;
      });

      response.on('end', async () => {
        const discussions = JSON.parse(
          data
        ).data.repository.discussions.edges;

        console.info('Discussions', discussions);

        resolve(discussions);
      });
    }).on('error', (error) => {
      console.log(error.message);
      reject(error);
    });

    const query = `
    query {
      repository(name: "reach-lang", owner: "reach-sh") {
        discussions(first: 100) {
          edges {
            node {
              objectID: title
              url: url
              title: title
            }
          }
        }
      }
    }
    `;

    request.write(JSON.stringify({ query }));
    request.end();
  });
};
const rawDiscussions = await retriveAllGitHubDiscussions();

const numberIndicatingDiscussionsClass = 4;
const discussions = rawDiscussions.map(({ node }) => {
  const { url, title } = node;
  return {
    objectID: url,
    pt: title,
    t: numberIndicatingDiscussionsClass,
    c: title,
  };
});

const indexJson = JSON.parse(await fs.readFile('./index.json'));
indexJson.push(...discussions);

const key = process.env['ALGOLIA_KEY'];
if ( ! key ) { throw Error(`No key!`); }
const client = algoliasearch('M53HHHS0ZW', key);
const index = client.initIndex('docs');

const addDataToAlgoliaBackend = (someIndex, dataToAdd) => {
  someIndex.saveObjects(dataToAdd).then(({ objectIDs }) => {
    console.log(objectIDs);
  }).catch(error => {
    console.log(error);
  });
};
addDataToAlgoliaBackend(index, discussions);

const now = Date.now();
const indexJson_lu = indexJson.map((x) => ({ ...x, lu: now }));

console.log('Running!', indexJson.length);
const r_puo = await index.partialUpdateObjects(indexJson_lu, {
  createIfNotExists: true,
});
console.log(r_puo);
const r_db = await index.deleteBy({
  numericFilters: [ `lu < ${now}` ],
});
console.log(r_db);
console.log('Done!');
