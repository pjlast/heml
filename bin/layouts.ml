let layout contents =
  {%heml|<head>
    <title>Hello, world!</title>
  </head>
  <body>
    <%s= contents %>
  </body>
|}
