function go
  if any-arguments $argv
    switch $argv[1]
      case ba
        cd ~/Documentos/BA/
      case rx
        cd ~/Documentos/BA/OB/order-book/royalty_exchange_project/
        . mysql-env.sh
      case ob
        cd ~/Documentos/BA/OB/order-book/royalty_exchange_project/react/orderbook/
      case vp
        cd ~/Documentos/BA/OB/order-book/royalty_exchange_project/react/vp/
      case cy
        go rx
        cd ~/Documentos/BA/OB/order-book/cypress
    end
  end
end
