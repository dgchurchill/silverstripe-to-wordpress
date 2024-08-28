(*
   
    silver-stripe-export Convert a Silverstripe site to Wordpress
    Copyright (C) 2024 David G. Churchill

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
    
*)

module Wordpress

open System.Net.Http
open System.Net.Http.Json
open WordPressPCL
open System.Threading.Tasks

type Config = {
    Site : string
    ClientId : string
    ClientSecret : string
    Username : string
    Password : string
}

type TokenResponse = {
    access_token : string
}

let createClient (config : Config) : Task<WordPressClient> =
    task {
        use httpClient = new HttpClient()

        use tokenRequest = new FormUrlEncodedContent(
            dict [
                "client_id", config.ClientId
                "client_secret", config.ClientSecret
                "grant_type", "password"
                "username", config.Username
                "password", config.Password
            ])

        let! tokenResponseMessage = httpClient.PostAsync("https://public-api.wordpress.com/oauth2/token", tokenRequest)
        let! tokenResponse = tokenResponseMessage.Content.ReadFromJsonAsync<TokenResponse>()

        let client = new WordPressClient($"https://public-api.wordpress.com/wp/v2/sites/{config.Site}/", defaultPath="")
        client.Auth.SetJWToken(tokenResponse.access_token) // auth method defaults to Bearer, and this sets the bearer token

        return client
    }

