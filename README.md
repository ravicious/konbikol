# konbikol

To run this thing in development mode:

```
yarn
yarn dev-server
```

To compile & commit an updated & minified version on GitHub Pages:

```
yarn release
```

To debug a ticket that's causing problems, put it in the root directory as `ticket.pdf`, run the dev
server and add `#debug` to the URL. This will make the app attempt to process `ticket.pdf`
immediately after page load.
