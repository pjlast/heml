let layout ~title contents =
  {%heml|<!DOCTYPE html>
<head>
  <title><%s= title %></title>
</head>
<body>
  <%raw= contents %>
</body>
|}
