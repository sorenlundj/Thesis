module Test where

import MMod
import Types

--import Test.Tasty
--import Test.Tasty.HUnit

ship    = Entity {eType = Ship,    eId = 001, eName = "Ship1",    eState = ["hehehe"]}
service = Entity {eType = Service, eId = 002, eName = "Service1", eState = []}
company = Entity {eType = Company, eId = 003, eName = "Company1", eState = []}

dep1 = Dependency {desc = "dep1", val = True}

relationShipService    = Relation {rFrom = ship,    rTo = service, dep = [dep1]}
relationServiceCompany = Relation {rFrom = service, rTo = company, dep = []}
relationServiceShip    = Relation {rFrom = service, rTo = ship,    dep = []}
relationCompanyService = Relation {rFrom = company, rTo = service, dep = []}

main = runFsm (checkout) NoEntsRels [SelectE ship
                                    ,SelectE service
                                    ,SelectE company
                                    ,SelectR relationShipService
                                    ,SelectR relationServiceCompany 
                                    ,SelectR relationServiceShip 
                                    ,SelectR relationCompanyService]

-- ved state change, aendr hele entityen








