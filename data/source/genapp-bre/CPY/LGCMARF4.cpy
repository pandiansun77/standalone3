      ******************************************************************        
      *                                                                *        
      * LICENSED MATERIALS - PROPERTY OF IBM                           *        
      *                                                                *        
      * "RESTRICTED MATERIALS OF IBM"                                  *        
      *                                                                *        
      * CB12                                                           *        
      *                                                                *        
      * (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED         *        
      *                                                                *        
      * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *        
      * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *        
      * CONTRACT WITH IBM CORPORATION                                  *        
      *                                                                *        
      *                                                                *        
      *               COPYBOOK for COMMAREA structure                  *        
      *                                                                *        
      *   This commarea can be used for all functions                  *        
      *                                                                *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           03 CA-REQUEST-ID            PIC X(6).                                
           03 CA-RETURN-CODE           PIC 9(2).                                
           03 CA-CUSTOMER-NUM          PIC 9(10).                               
      *    Fields used in INQ, UPD, ADD & DELETE policy                         
           03 CA-POLICY-REQUEST.                                                
              05 CA-POLICY-NUM         PIC 9(10).                               
      *       Common policy details                                             
              05 CA-POLICY-COMMON.                                              
                 07 CA-ISSUE-DATE      PIC X(10).                               
                 07 CA-EXPIRY-DATE     PIC X(10).                               
                 07 CA-LASTCHANGED     PIC X(26).                               
                 07 CA-BROKERID        PIC 9(10).                               
                 07 CA-BROKERSREF      PIC X(10).                               
                 07 CA-PAYMENT         PIC 9(6).                                
      *       Commercial policy description                                     
              05 CA-COMMERCIAL.                                                 
                 07 CA-B-Address         PIC X(255).                            
                 07 CA-B-Postcode        PIC X(8).                              
                 07 CA-B-Latitude        PIC X(11).                             
                 07 CA-B-Longitude       PIC X(11).                             
                 07 CA-B-Customer        PIC X(255).                            
                 07 CA-B-PropType        PIC X(255).                            
                 07 CA-B-FirePeril       PIC 9(4).                              
                 07 CA-B-FirePremium     PIC 9(8).                              
                 07 CA-B-CrimePeril      PIC 9(4).                              
                 07 CA-B-CrimePremium    PIC 9(8).                              
                 07 CA-B-FloodPeril      PIC 9(4).                              
                 07 CA-B-FloodPremium    PIC 9(8).                              
                 07 CA-B-WeatherPeril    PIC 9(4).                              
                 07 CA-B-WeatherPremium  PIC 9(8).                              
                 07 CA-B-Status          PIC 9(4).                              
                 07 CA-B-RejectReason    PIC X(255).                            
                 07 CA-B-FILLER          PIC X(31298).                          
