import algoliasearch from 'algoliasearch';
import fs from 'fs/promises';
import https from 'https';

const retriveAllGitHubDiscussions = (lastCursor) => {
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
        const json = JSON.parse(data);
        if (!json.data || !json.data.repository) {
          console.error('Bad response from GH', `lastCursor=${lastCursor}`, json);
          reject();
        }
        const { edges } = json.data.repository.discussions;
        console.info('Discussions', edges);
        const cursorOfLastDiscussion = edges[
          edges.length - 1
        ]?.cursor;
        resolve([ edges, cursorOfLastDiscussion ]);
      });
    }).on('error', (error) => {
      reject(error);
    });

    if (lastCursor) {
      lastCursor = `"${lastCursor}"`;
    }
    const query = `query {
      repository(name: "reach-lang", owner: "reach-sh") {
        discussions(
          first: 100
          after: ${lastCursor}
          orderBy: {field: CREATED_AT, direction: ASC}) {
            edges {
              node {
                objectID: title
                url: url
                title: title
              }
              cursor
            }}}}`;

    request.write(JSON.stringify({ query }));
    request.end();
  });
};
const rawDiscussions = [];
let [
  nextDiscussions,
  cursor,
] = await retriveAllGitHubDiscussions(null);
while (nextDiscussions.length) {
  rawDiscussions.push(...nextDiscussions);
  [
    nextDiscussions,
    cursor,
  ] = await retriveAllGitHubDiscussions(cursor);
}

const sd_ghd = 4;
const discussions = rawDiscussions.map(({ node }) => {
  const { url, title } = node;
  return {
    objectID: url,
    pt: title,
    t: sd_ghd,
    c: title,
  };
});

const indexJson = JSON.parse(await fs.readFile('./index.json'));
indexJson.push(...discussions);

const key = process.env['ALGOLIA_KEY'];
if ( ! key ) { throw Error(`No key!`); }
const client = algoliasearch('M53HHHS0ZW', key);
const index = client.initIndex('docs');

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
