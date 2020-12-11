function load-dump
  if any-arguments $argv
    set -l DUMP_FILE ~/Documentos/BA/OB/mysql-dumps/$argv[1]
    if [ -f "$DUMP_FILE" ]
      docker exec -i order-book_mysql_1 /usr/bin/mysql -u root --password=root-password auctions < $DUMP_FILE
    end
  end
end
