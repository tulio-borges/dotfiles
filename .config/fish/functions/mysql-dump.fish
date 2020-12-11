function mysql-dump
  if any-arguments $argv
    docker exec order-book_mysql_1 /usr/bin/mysqldump -u root --password=root-password auctions >  ~/Documentos/BA/OB/mysql-dumps/$argv[1]
  end
end
