{-# LANGUAGE OverloadedStrings #-}

module Test where

import MMod
import Types

--import Test.Tasty
--import Test.Tasty.HUnit

ship    = Entity {entityType = Ship,    entityId = 001, entityName = "Ship1",    entityState = ["hehehe"]}
service = Entity {entityType = Service, entityId = 002, entityName = "Service1", entityState = []}
company = Entity {entityType = Company, entityId = 003, entityName = "Company1", entityState = []}

dep1 = Dependency {name = "dep1", dep = True}

relationShipService    = Relation {relationFrom = ship,    relationTo = service, dependency = [dep1]}
relationServiceCompany = Relation {relationFrom = service, relationTo = company, dependency = []}
relationServiceShip    = Relation {relationFrom = service, relationTo = ship,    dependency = []}
relationCompanyService = Relation {relationFrom = company, relationTo = service, dependency = []}

main = runFsm (checkout) NoEntsRels [SelectE ship
                                    ,SelectE service
                                    ,SelectE company
                                    ,SelectR relationShipService
                                    ,SelectR relationServiceCompany 
                                    ,SelectR relationServiceShip 
                                    ,SelectR relationCompanyService]
--









